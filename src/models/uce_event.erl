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

-export([add/2, get/2, exists/2, list/9]).

-include("uce.hrl").
-include("uce_models.hrl").
-include("uce_async.hrl").

add(Domain, #uce_event{id=none}=Event) ->
    ?MODULE:add(Domain, Event#uce_event{id=utils:random()});
add(Domain, #uce_event{datetime=none}=Event) ->
    ?MODULE:add(Domain, Event#uce_event{datetime=utils:now()});
add(Domain, #uce_event{location=Location, type=Type, id=Id, from=From} = Event) ->
    case location_helpers:exists(Domain, Location) of
	false ->
	    {error, not_found};
	true ->
	    case ?DB_MODULE:add(Domain, Event) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
                ?PUBSUB_MODULE:publish(Domain, Location, Type, From, Id),
                ?SEARCH_MODULE:add(Domain, Event),
                case catch triggers:run(Location, Type, Event) of
                    {error, Reason} ->
                        ?DEBUG("Error : ~p~n", [{error, Reason}]);
                    {'EXIT', Reason} ->
                        ?DEBUG("Error : ~p~n", [{error, Reason}]);
                    _ ->
                        nothing
                end,
                {ok, Id}
	    end
    end.

get(Domain, Id) ->
    ?DB_MODULE:get(Domain, Id).

exists(Domain, Id) ->
    case Id of
	"" -> % "" is the root of the event hierarchy
	    true;
	_ ->
	    case ?MODULE:get(Domain, Id) of
		{error, _} ->
		    false;	       
		_ ->
		    true
	    end
    end.
		    
list(_, _, _, _, [], _, _, _, _) ->
    {ok, []};
list(Domain, Location, Search, From, '_', Uid, Start, End, Parent) ->
    ?MODULE:list(Domain, Location, Search, From, ['_'], Uid, Start, End, Parent);
list(Domain, Location, Search, From, [Type|Tail], Uid, Start, End, Parent) ->
    {ok, AllEvents} = case Search of
                          '_' ->
                              ?DB_MODULE:list(Domain, Location, From, Type, Start, End, Parent);
                          _ ->
                              ?SEARCH_MODULE:list(Domain, Location, Search, From, Type, Start, End, Parent)
		end,
    FilteredEvents = lists:filter(fun(#uce_event{to=To}) ->
					  if
					      To == "all" ->
						  true;
					      To == Uid ->
						  true;
					      true ->
						  false
					  end
				  end,
				  AllEvents),
    case ?MODULE:list(Domain, Location, Search, From, Tail, Uid, Start, End, Parent) of
	{error, Reason} ->
	    {error, Reason};
	{ok, RemainingEvents} ->
	    {ok, FilteredEvents ++ RemainingEvents}
    end.
