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
-module(uce_meeting_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_meeting).

-export([add/2,
         delete/2,
         get/2,
         update/2,
         list/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(_Domain, #uce_meeting{} = Meeting) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_meeting", to_collection(Meeting)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

delete(_Domain, [Meeting]) ->
    case emongo:delete(?MONGO_POOL, "uce_meeting", [{"meeting", Meeting}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, deleted}
    end.

get(_Domain, [Meeting]) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_meeting",
                               [{"meeting", Meeting}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[Record] ->
	    {ok, from_collection(Record)};
	_ ->
	    {error, not_found}
    end.

update(_Domain, #uce_meeting{id=[MeetingName]} = Meeting) ->
    case catch emongo:update_sync(?MONGO_POOL, "uce_meeting",
                                  [{"meeting", MeetingName}],
                                  to_collection(Meeting), false) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, updated}
    end.

list(_Domain) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_meeting", []) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    Meetings = lists:map(fun(Collection) ->
					 from_collection(Collection)
				 end,
				 Collections),
	    {ok, Meetings}
    end.


to_collection(#uce_meeting{id=[MeetingName],
			     start_date=Start,
			     end_date=End,
			     roster=Roster,
			     metadata=Metadata}) ->
    [{"meeting", MeetingName},
     {"start_date", integer_to_list(Start)},
     {"end_date", integer_to_list(End)},
     {"roster", Roster},
     {"metadata", Metadata}].

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["meeting", "start_date", "end_date", "roster", "metadata"]) of
	[Meeting, Start, End, Roster, Metadata] ->
	    #uce_meeting{id=[Meeting],
			   start_date=list_to_integer(Start),
			   end_date=list_to_integer(End),
			   roster=Roster,
			   metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.
