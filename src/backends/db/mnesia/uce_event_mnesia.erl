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

-export([add/1,
         get/2,
         list/6]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_event,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, uce_event)}]).

add(#uce_event{} = Event) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(Event)
                            end) of
        {atomic, _} ->
            {ok, Event#uce_event.id};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

get(_Domain, Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:read(uce_event, Id)
                            end) of
        {aborted, _} ->
            throw({error, bad_parameters});
        {atomic, []} ->
            throw({error, not_found});
        {atomic, [Event]} ->
            {ok, Event}
    end.

list(Location, From, [], Start, End, Parent) ->
    ?MODULE:list(Location, From, [""], Start, End, Parent);
list(Location, From, Types, Start, End, Parent) ->
    {SelectLocation, ResultLocation} =
        case Location of
            {"", _} ->
                {'$4', '$4'};
            _ ->
                {Location, {Location}}
        end,
    {SelectFrom, ResultFrom} =
        case From of
            {"", _} ->
                {'$5', '$5'};
            _ ->
                {From, {From}}
        end,
    SelectParent = if
                       Parent == "" ->
                           '$8';
                       true ->
                           Parent
                   end,
    Guard = if
                Start /= 0, End /= infinity ->
                    [{'>=', '$3', Start}, {'=<', '$3', End}];

                Start /= 0 ->
                    [{'>=', '$3', Start}];

                End /= infinity ->
                    [{'=<', '$3', End}];

                true ->
                    []
            end,

    Events =
        lists:map(fun(Type) ->
                          SelectType = if
                                           Type == "" ->
                                               '$7';
                                           true ->
                                               Type
                                       end,

                          Match = #uce_event{id='$1',
                                             domain='$2',
                                             datetime='$3',
                                             location=SelectLocation,
                                             from=SelectFrom,
                                             to='$6',
                                             type=SelectType,
                                             parent=SelectParent,
                                             metadata='$9'},
                          Result = {{'uce_event', '$1', '$2', '$3', ResultLocation,
                                     ResultFrom, '$6', SelectType, SelectParent, '$9'}},
                          mnesia:dirty_select(uce_event, [{Match, Guard, [Result]}])
                  end,
                  Types),
    {ok, lists:flatten(Events)}.

drop() ->
    mnesia:clear_table(uce_event).
