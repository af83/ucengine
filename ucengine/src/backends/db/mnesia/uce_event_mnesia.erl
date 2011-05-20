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
-module(uce_event_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_event).

-export([init/0, drop/0]).

-export([add/2,
         get/2,
         list/7]).

-include("uce.hrl").

init() ->
    case mnesia:create_table(uce_event,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_event)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_event}} -> ok
    end.

add(_Domain, #uce_event{} = Event) ->
    case mnesia:dirty_write(Event) of
        ok ->
            {ok, Event#uce_event.id};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

get(_Domain, Id) ->
    case mnesia:dirty_read(uce_event, Id) of
        {aborted, _} ->
            throw({error, bad_parameters});
        [] ->
            throw({error, not_found});
        [Event] ->
            {ok, Event}
    end.

list(Location, From, [], Start, End, Parent, Order) ->
    list(Location, From, [""], Start, End, Parent, Order);
list(Location, From, Types, Start, End, Parent, Order) ->
    {SelectLocation, ResultLocation} =
        case Location of
            {"", Domain} ->
                {{'$3', Domain}, {{'$3', Domain}}};
            _ ->
                {Location, {Location}}
        end,
    {SelectFrom, ResultFrom} =
        case From of
            {"", _} ->
                {'$4', '$4'};
            _ ->
                {From, {From}}
        end,
    SelectParent = if
                       Parent == "" ->
                           '$7';
                       true ->
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

                          Match = #uce_event{id='$1',
                                             datetime='$2',
                                             location=SelectLocation,
                                             from=SelectFrom,
                                             to='$5',
                                             type=SelectType,
                                             parent=SelectParent,
                                             metadata='$8'},
                          Result = {{'uce_event', '$1', '$2', ResultLocation,
                                     ResultFrom, '$5', SelectType, SelectParent, '$8'}},
                          mnesia:dirty_select(uce_event, [{Match, Guard, [Result]}])
                  end,
                  Types),
    %% XXX: mnesia should be able to sort events
    OrderedEvents = event_helpers:sort(lists:flatten(Events), Order),
    {ok, OrderedEvents}.

drop() ->
    mnesia:clear_table(uce_event).
