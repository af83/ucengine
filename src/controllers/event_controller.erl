%%
%%  U.C.Engine - Unified Colloboration Engine
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

-export([init/0, get/4, list/4, add/4]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/event/?([^/]+)?",
                callbacks=[{?MODULE, add,
                            ["uid", "sid", "type", "to", "parent", "metadata"],
                            [required, required, required, "", "", []],
                            [string, string, string, string, string, dictionary]}]},

     #uce_route{method='GET',
                regexp="/event/([^/]+)/([^/]+)",
                callbacks=[{?MODULE, get,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},     

     #uce_route{method='GET',
                regexp="/event/?([^/]+)?",
                callbacks=[{?MODULE, list,
                            ["uid",
                             "sid",
                             "search",
                             "type",
                             "from",
                             "start",
                             "end",
                             "count",
                             "page",
                             "order",
                             "parent",
                             "_async"],
                            [required, required, '_', '_', "", 0, infinity, infinity, 1, asc, '_', "no"],
                            [string,
                             string,
                             [string, atom],
                             [string, atom],
                             string,
                             integer,
                             [integer, atom],
                             [integer, atom],
                             integer,
                             atom,
                             [string, atom],
                             string]}]}].


add(Domain, [], Params, Arg) ->
    ?MODULE:add(Domain, [""], Params, Arg);
add(Domain, [Meeting], [Uid, Sid, Type, To, Parent, Metadata], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "event", "add", {Meeting, Domain}, [{"type", Type},
                                                                                   {"to", To}]),
    {ok, Id} = uce_event:add(#uce_event{domain=Domain,
                                        location={Meeting, Domain},
                                        from={Uid, Domain},
                                        type=Type,
                                        to={To, Domain},
                                        parent=Parent,
                                        metadata=Metadata}),
    json_helpers:created(Id).

get(Domain, [_, Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "event", "get", {"", ""}, [{"id", Id}]),
    {ok, #uce_event{to=To} = Event} = uce_event:get(Id),
    case To of
        {"", _} ->
            json_helpers:json(event_helpers:to_json(Event));
        {Uid, Domain} ->
            json_helpers:json(event_helpers:to_json(Event));
        _ ->
            throw({error, unauthorized})
    end.

list(Domain, [], Params, Arg) ->
    ?MODULE:list(Domain, [""], Params, Arg);
list(Domain, [Meeting],
     [Uid, Sid, Search, Type, From, Start, End, Count, Page, Order, Parent, Async], Arg) ->

    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "event", "list", {Meeting, Domain}, [{"from", From}]),
    Types = case Type of
                '_' ->
                    ['_'];
                _ ->
                    string:tokens(Type, ",")
            end,
    Keywords = case Search of
                   '_' ->
                       '_';
                   _ ->
                       string:tokens(Search, " ")
               end,
    case uce_event:list({Meeting, Domain},
                        Keywords,
                        {From, Domain},
                        Types,
                        {Uid, Domain},
                        Start,
                        End,
                        Parent) of
        {ok, []} ->
            case Async of
                "no" ->
                    json_helpers:json(event_helpers:to_json([]));
                "lp" ->
                    uce_async_lp:wait({Meeting, Domain},
                                      Keywords,
                                      {From, Domain},
                                      Types,
                                      {Uid, Domain},
                                      Start,
                                      End,
                                      Parent,
                                      Arg#arg.clisock);
                _ ->
                    {error, bad_parameters}
            end;
        {ok, Events} ->
            EventPage = helpers:paginate(event_helpers:sort(Events), Count, Page, Order),
            json_helpers:json(event_helpers:to_json(EventPage))
    end.
