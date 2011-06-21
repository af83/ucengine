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
-module(uce_event).

-author('tbomandouki@af83.com').

-export([add/2, get/2, exists/2, list/12, search/12]).

-include("uce.hrl").

add(Domain, #uce_event{id={none, Domain}}=Event) ->
    add(Domain, Event#uce_event{id={utils:random(), Domain}});
add(Domain, #uce_event{datetime=undefined}=Event) ->
    add(Domain, Event#uce_event{datetime=utils:now()});
add(Domain, #uce_event{location={Location, Domain}, from=From, to=To, parent=Parent} = Event) ->
    LocationExists = uce_meeting:exists(Domain, Location),
    FromExists = uce_user:exists(Domain, From),
    ToExists = uce_user:exists(Domain, To),
    ParentExists = uce_event:exists(Domain, {Parent, Domain}),

    if
        LocationExists, FromExists, ToExists, ParentExists ->
            {ok, Id} = (db:get(?MODULE, Domain)):add(Domain, Event),
            ?PUBSUB_MODULE:publish(Event),
            ?SEARCH_MODULE:add(Event),
            ?COUNTER("event_add:" ++ Event#uce_event.type),
            {ok, Id};
        true ->
            throw({error, not_found})
    end.

get(Domain, Id) ->
    (db:get(?MODULE, Domain)):get(Domain, Id).

exists(Domain, Id) ->
    case Id of
        {"", _Domain} -> true;
        _ ->
            case catch get(Domain, Id) of
                {error, not_found} ->
                   false;
                {error, Reason} ->
                    throw({error, Reason});
                _ ->
                    true
            end
    end.

filter_private(Events, {Name, Domain}) ->
    lists:filter(fun(#uce_event{to=To, from=From}) ->
                         if
                             To == {"", ""} ->     % all
                                 true;
                             To == {"", Domain} -> % all
                                 true;
                             To == {Name, Domain} ->
                                 true;
                             From == {Name, Domain} ->
                                 true;
                             true ->
                                 false
                         end
                 end,
                 Events).

search(Domain, Location, Search, From, Types, Uid, DateStart, DateEnd, Parent, Start, Max, Order) ->
    {ok, NumTotal, Events} = ?SEARCH_MODULE:list(Domain,
                                                 Location,
                                                 Search,
                                                 From,
                                                 Types,
                                                 DateStart,
                                                 DateEnd,
                                                 Parent,
                                                 Start,
                                                 Max,
                                                 Order),
    ?COUNTER(event_search),
    {ok, NumTotal, filter_private(Events, Uid)}.

list(Domain, Location, Search, From, Types, Uid, DateStart, DateEnd, Parent, Start, Max, Order) ->
    N = now(),
    {ok, _Num, Events} = uce_event_erlang_search:list(Domain,
                                                      Location,
                                                      Search,
                                                      From,
                                                      Types,
                                                      DateStart,
                                                      DateEnd,
                                                      Parent,
                                                      Start,
                                                      Max,
                                                      Order),
    ?TIMER_APPEND(event_list, N),
    ?COUNTER(event_list),
    {ok, filter_private(Events, Uid)}.
