%%
%%  U.C.Engine - Unified Collaboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(event_controller).

-export([init/0, get/5, list/5, add/5, add2/5, live/5]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{method='POST',
                path=["event", '...'],
                content_type="application/json",
                callback={?MODULE, add2}},

     #uce_route{method='POST',
                path=["event", '...'],
                middlewares = [auth,
                               {params, [{"type", required, string},
                                         {"to", "", string},
                                         {"parent", "", string},
                                         {"metadata", [], dictionary}]}],
                callback={?MODULE, add}},

     #uce_route{method='GET',
                path=["event", meeting, id],
                middlewares = [auth],
                callback={?MODULE, get}},

     #uce_route{method='GET',
                path=["live", '...'],
                middlewares = [auth,
                               {params, [{"search", "", string},
                                         {"type", "", string},
                                         {"from", "", string},
                                         {"start", 0, integer},
                                         {"parent", "", string},
                                         {"mode", "longpolling", string}]}],
                callback={?MODULE, live}},

     #uce_route{method='GET',
                path=["event", '...'],
                middlewares = [auth,
                               {params, [{"search", "", string},
                                         {"type", "", string},
                                         {"from", "", string},
                                         {"start", 0, integer},
                                         {"end", infinity, integer},
                                         {"count", infinity, integer},
                                         {"page", 1, integer},
                                         {"order", asc, atom},
                                         {"parent", "", string}]}],
                callback={?MODULE, list}}].

add(Domain, [], Params, Request, Response) ->
    add(Domain, [""], Params, Request, Response);
add(Domain, [Meeting], [Uid, _Sid, Type, To, Parent, Metadata], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "event", "add",
                                   [{"type", Type}, {"to", To}]),
    case Type of
        "internal."++ _Rest ->
            throw({error, unauthorized});
        _OtherEvent ->
            {ok, Id} = uce_event:add(Domain,
                                     #uce_event{id=none,
                                                location=Meeting,
                                                from=Uid,
                                                type=Type,
                                                to=To,
                                                parent=Parent,
                                                metadata=json_helpers:to_struct(Metadata)}),
            json_helpers:created(Response, Id)
    end.

add2(Domain, [], [], Request, Response) ->
    add2(Domain, [""], [], Request, Response);
add2(Domain, [Meeting], [], #uce_request{arg=Arg}, Response) ->
    {struct, Json} = mochijson:decode(Arg#arg.clidata),
    Uid = proplists:get_value("uid", Json),
    Sid = proplists:get_value("sid", Json),
    ok = uce_presence:assert(Domain, Uid, Sid),

    Type = proplists:get_value("type", Json),
    To = proplists:get_value("to", Json, ""),
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "event", "add",
                                   [{"type", Type}, {"to", To}]),
    Parent = proplists:get_value("parent", Json, ""),
    Metadata = proplists:get_value("metadata", Json, []),
    {ok, Id} = uce_event:add(Domain,
                             #uce_event{id=none,
                                        location=Meeting,
                                        from=Uid,
                                        type=Type,
                                        to=To,
                                        parent=Parent,
                                        metadata=Metadata}),
    json_helpers:created(Response, Id).

get(Domain, [_, {id, Id}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "event", "get", [{"id", Id}]),
    {ok, #uce_event{to=To} = Event} = uce_event:get(Domain, Id),
    case To of
        "" ->
            json_helpers:json(Response, Domain, Event);
        Uid ->
            json_helpers:json(Response, Domain, Event);
        _ ->
            throw({error, unauthorized})
    end.

list(Domain, [], Params, Request, Response) ->
    list(Domain, [""], Params, Request, Response);
list(Domain, [Meeting],
     [Uid, _Sid, Search, Type, From, DateStart, DateEnd, Count, Page, Order, Parent], _Request, Response) ->

    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "event", "list", [{"from", From}]),

    Keywords = string:tokens(Search, ","),
    Types = string:tokens(Type, ","),

    Start = uce_paginate:index(Count, 0, Page),
    {ok, Events} = uce_event:list(Domain,
                                  Meeting,
                                  Keywords,
                                  From,
                                  Types,
                                  Uid,
                                  DateStart,
                                  DateEnd,
                                  Parent,
                                  Start,
                                  Count,
                                  Order),
    json_helpers:json(Response, Domain, Events).

live(Domain, [], Params, Request, Response) ->
    live(Domain, [""], Params, Request, Response);
live(Domain, [Meeting],
     [Uid, Sid, Search, Type, From, UserStart, Parent, Mode], Request, Response) ->

    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "event", "list", [{"from", From}]),

    Start = case get_last_event_id(Request) of
                undefined ->
                    UserStart;
                Value ->
                    Value + 1
            end,

    Keywords = string:tokens(Search, ","),
    Types = string:tokens(Type, ","),

    {ok, PreviousEvents} = uce_event:list(Domain,
                                          Meeting,
                                          Uid,
                                          Keywords,
                                          From,
                                          Types,
                                          Start,
                                          Parent),
    case Mode of
        "longpolling" ->
            uce_async_lp:wait(Response,
                              Domain,
                              Uid,
                              Meeting,
                              Keywords,
                              From,
                              Types,
                              Parent,
                              PreviousEvents);
        "eventsource" ->
            uce_async_stream:wait(Response,
                                  Domain,
                                  Uid,
                                  Meeting,
                                  Keywords,
                                  From,
                                  Types,
                                  Parent,
                                  Sid,
                                  PreviousEvents);
        _ ->
            {error, bad_parameters}
    end.

get_last_event_id(#uce_request{arg=Arg}) ->
    Header = "Last-Event-Id",
    case lists:keyfind(Header, 3, Arg#arg.headers#headers.other) of
        false ->
            undefined;
        {http_header, _, Header, _, ""} ->
            undefined;
        {http_header, _, Header, _, Value} ->
            list_to_integer(Value)
    end.
