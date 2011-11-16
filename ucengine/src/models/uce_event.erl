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
-module(uce_event).

-export([add/2, get/2, exists/2, list/8, list/12, search/12]).

-include("uce.hrl").

-spec add(domain(), event()) -> {ok, event_id()} | erlang:throw({error, not_found}).
add(Domain, #uce_event{id=none}=Event) ->
    add(Domain, Event#uce_event{id=utils:random()});
add(Domain, #uce_event{datetime=undefined}=Event) ->
    add(Domain, Event#uce_event{datetime=utils:now()});
add(Domain, #uce_event{location=Location, to=To, parent=Parent} = Event) ->
    LocationExists = uce_meeting:exists(Domain, Location),
    ToExists = uce_user:exists(Domain, To),
    ParentExists = uce_event:exists(Domain, Parent),

    case {LocationExists, ToExists, ParentExists} of
        {true, true, true} ->
            {ok, Id} = (db:get(?MODULE, Domain)):add(Domain, Event),
            uce_meeting:publish(Domain, Event),
            ?SEARCH_MODULE:add(Domain, Event),
            ?COUNTER("event_add:" ++ Event#uce_event.type),
            {ok, Id};
        _ ->% [TODO] throw the missing exist
            throw({error, not_found})
    end.

-spec get(domain(), event_id()) -> {ok, event()} | erlang:throw({error, bad_parameters}).
get(Domain, Id) ->
    (db:get(?MODULE, Domain)):get(Domain, Id).

-spec exists(domain(), event_id()) -> boolean().
exists(_Domain, "") ->
    true;
exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        _ ->
            true
    end.

-spec filter_private(list(event()), uid()) -> list(event()).
filter_private(Events, Name) ->
    lists:filter(fun(#uce_event{to=To, from=From}) ->
                         if
                             To == "" ->     % all
                                 true;
                             To == Name ->
                                 true;
                             From == Name ->
                                 true;
                             true ->
                                 false
                         end
                 end,
                 Events).

-spec search(Domain    :: domain(),
             Location  :: meeting_id(),
             Search    :: string(),
             From      :: uid(),
             Types     :: string(),
             Uid       :: uid(),
             DateStart :: timestamp(),
             DateEnd   :: timestamp(),
             Parent    :: event_id(),
             Start     :: integer(),
             Max       :: integer(),
             Order     :: string()) -> {ok, integer(), list(event())}.
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

list(Domain, Location, Uid, Search, From, Types, Start, Parent) ->
    list(Domain, Location, Search, From, Types, Uid, Start, infinity, Parent, 0, infinity, asc).
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
