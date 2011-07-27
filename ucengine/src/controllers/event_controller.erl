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

-export([init/0, get/4, list/4, add/4, add2/4, live/4]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{method='POST',
                path=["event", '...'],
                content_type="application/json",
                callback={?MODULE, add2, []}},

     #uce_route{method='POST',
                path=["event", '...'],
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"type", required, string},
                           {"to", "", string},
                           {"parent", "", string},
                           {"metadata", [], dictionary}]}},

     #uce_route{method='GET',
                path=["event", meeting, id],
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='GET',
                path=["live", '...'],
                callback={?MODULE, live,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"search", "", string},
                           {"type", "", string},
                           {"from", "", string},
                           {"start", 0, integer},
                           {"parent", "", string},
                           {"mode", "longpolling", string}]}},

     #uce_route{method='GET',
                path=["event", '...'],
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"search", "", string},
                           {"type", "", string},
                           {"from", "", string},
                           {"start", 0, integer},
                           {"end", infinity, [integer, atom]},
                           {"count", infinity, [integer, atom]},
                           {"page", 1, integer},
                           {"order", asc, atom},
                           {"parent", "", string}]}}].

add(Domain, [], Params, Arg) ->
    add(Domain, [""], Params, Arg);
add(Domain, [Meeting], [Uid, Sid, Type, To, Parent, Metadata], _Arg) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
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
            json_helpers:created(Domain, Id)
    end.

add2(Domain, [Meeting], [], Arg) ->
    {struct, Json} = mochijson:decode(Arg#arg.clidata),
    Uid = proplists:get_value("uid", Json),
    Sid = proplists:get_value("sid", Json),
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),

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
    json_helpers:created(Domain, Id).

get(Domain, [_, {id, Id}], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "event", "get", [{"id", Id}]),
    {ok, #uce_event{to=To} = Event} = uce_event:get(Domain, Id),
    case To of
        "" ->
            json_helpers:json(Domain, Event);
        Uid ->
            json_helpers:json(Domain, Event);
        _ ->
            throw({error, unauthorized})
    end.

list(Domain, [], Params, Arg) ->
    list(Domain, [""], Params, Arg);
list(Domain, [Meeting],
     [Uid, Sid, Search, Type, From, DateStart, DateEnd, Count, Page, Order, Parent], _Arg) ->

    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
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
    json_helpers:json(Domain, Events).

live(Domain, [], Params, Arg) ->
    live(Domain, [""], Params, Arg);
live(Domain, [Meeting],
     [Uid, Sid, Search, Type, From, UserStart, Parent, Mode], Arg) ->

    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "event", "list", [{"from", From}]),

    Start = case get_last_event_id(Arg) of
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
            uce_async_lp:wait(Domain,
                              Meeting,
                              Keywords,
                              From,
                              Types,
                              Parent,
                              PreviousEvents);
        "eventsource" ->
            uce_async_stream:wait(Domain,
                                  Meeting,
                                  Keywords,
                                  From,
                                  Types,
                                  Parent,
                                  PreviousEvents);
        _ ->
            {error, bad_parameters}
    end.

get_last_event_id(Arg) ->
    Header = "Last-Event-Id",
    case lists:keyfind(Header, 3, Arg#arg.headers#headers.other) of
        false ->
            undefined;
        {http_header, _, Header, _, Value} ->
            list_to_integer(Value)
    end.
