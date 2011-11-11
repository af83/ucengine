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
-module(uce_event_mnesia).

-behaviour(gen_uce_event).

-export([init/0, drop/0]).

-export([add/2,
         get/2,
         list/8]).

-include("uce.hrl").

init() ->
    case mnesia:create_table(uce_event,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_event)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_event}} -> ok
    end.

add(Domain, #uce_event{id=Id} = Event) ->
    ok = mnesia:dirty_write(Event#uce_event{id={Id, Domain}}),
    {ok, Id}.

get(Domain, Id) ->
    case mnesia:dirty_read(uce_event, {Id, Domain}) of
        [] ->
            throw({error, not_found});
        [Event] ->
            {ok, remove_domain_from_id(Event)}
    end.

list(Domain, Location, From, [], Start, End, Parent, Order) ->
    list(Domain, Location, From, [""], Start, End, Parent, Order);
list(Domain, Location, From, Types, Start, End, Parent, Order) ->
    {SelectId, ResultId} = {{'$1', Domain}, {{'$1', Domain}}},
    {SelectLocation, ResultLocation} =
        case Location of
            "" ->
                {'$3', '$3'};
            _ ->
                {Location, Location}
        end,
    {SelectFrom, ResultFrom} =
        case From of
            "" ->
                {'$4', '$4'};
            _ ->
                {From, From}
        end,
    SelectParent =
        case Parent of
            "" ->
                '$7';
            _ ->
                Parent
        end,
    Guard = if
                Start /= 0, End /= infinity ->
                    [{'>=', '$2', Start}, {'=<', '$2', End}];

                Start /= 0 ->
                    [{'>=', '$2', Start}];

                End /= infinity ->
                    [{'=<', '$2', End}];

                true ->
                    []
            end,

    Events =
        lists:map(fun(Type) ->
                          SelectType = if
                                           Type == "" ->
                                               '$6';
                                           true ->
                                               Type
                                       end,

                          Match = #uce_event{id=SelectId,
                                             datetime='$2',
                                             location=SelectLocation,
                                             from=SelectFrom,
                                             to='$5',
                                             type=SelectType,
                                             parent=SelectParent,
                                             metadata='$8'},
                          Result = {{'uce_event', ResultId, '$2', ResultLocation,
                                     ResultFrom, '$5', SelectType, SelectParent, '$8'}},
                          mnesia:dirty_select(uce_event, [{Match, Guard, [Result]}])
                  end,
                  Types),
    %% XXX: mnesia should be able to sort events
    OrderedEvents = event_helpers:sort(lists:flatten(Events), Order),
    {ok, remove_domain_from_id(OrderedEvents)}.

drop() ->
    mnesia:clear_table(uce_event).

remove_domain_from_id(Events) ->
    ?REMOVE_ID_FROM_RECORD(Events, uce_event).
