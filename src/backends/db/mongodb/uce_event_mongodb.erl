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
-module(uce_event_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_event).

-export([add/1,
	 get/1,
	 list/6]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_event{} = Event) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_event", to_collection(Event)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

get(Id) ->
    case emongo:find_one(?MONGO_POOL, "uce_event", [{"id", Id}]) of
	[Collection] ->
	    {ok, from_collection(Collection)};
	_ ->
	    {error, not_found}
    end.

list(Location, From, Type, Start, End, Parent) ->
    SelectLocation = case Location of
			 [""] ->
			     [];
			 [Meeting] ->
			     [{"meeting", Meeting}]
		     end,
    SelectFrom = if
		       From  == '_' ->
			   [];
		       true ->
			   [{"from", From}]
		   end,
    SelectType = if
		       Type == '_' ->
			   [];
		       true ->
			   [{"type", Type}]
		   end,
    SelectParent = if
		       Parent == '_' ->
			   [];
		       true ->
			   [{"parent", Type}]
		   end,
    SelectTime = if
		       Start == 0, End == infinity -> 
			   [];
		       Start /= 0, End == infinity ->
			   [{"datetime", [{'>=', Start}]}];
		       Start /= 0, End /= infinity ->
			   [{"datetime", [{'>=', Start},
					  {'=<', End}]}];
		       Start == 0, End /= infinity ->
			   [{"datetime", [{'=<', End}]}];
		       true ->
			   []
	       end,
    Events = lists:map(fun(Collection) ->
			       from_collection(Collection)
		       end,
		       emongo:find_all(?MONGO_POOL,"uce_event",
				       SelectLocation ++
					   SelectFrom ++
					   SelectType ++
					   SelectParent ++
					   SelectTime,
				       [{orderby, [{"this.datetime", asc}]}])),
    {ok, Events}.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		  ["id", "meeting", "from", "metadata", "datetime", "type", "parent", "to"]) of
	[Id, Meeting, From, Metadata, Datetime, Type, Parent, To] ->
	    #uce_event{id=Id,
		       datetime=Datetime,
		       from=From,
		       to=To,
		       location=[Meeting],
		       type=Type,
		       parent=Parent,
		       metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_event{id=Id,
			 location=[Meeting],
			 from=From,
			 to=To,
			 metadata=Metadata,
			 datetime=Datetime,
			 type=Type,
			 parent=Parent}) ->
    [{"id", Id},
     {"meeting", Meeting},
     {"from", From},
     {"to", To},
     {"metadata", Metadata},
     {"datetime", Datetime},
     {"type", Type},
     {"parent", Parent}].
