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

-export([add/1,
         delete/1,
         get/1,
         update/1,
         list/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_meeting{} = Meeting) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_meeting", to_collection(Meeting)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, created}
    end.

delete({Name, Domain}) ->
    case emongo:delete(?MONGO_POOL, "uce_meeting", [{"name", Name}, {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, deleted}
    end.

get({Name, Domain}) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_meeting",
                               [{"name", Name}, {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Record] ->
            {ok, from_collection(Record)};
        _ ->
            throw({error, not_found})
    end.

update(#uce_meeting{id={Name, Domain}} = Meeting) ->
    case catch emongo:update_sync(?MONGO_POOL, "uce_meeting",
                                  [{"name", Name}, {"domain", Domain}],
                                  to_collection(Meeting), false) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, updated}
    end.

list(Domain) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_meeting", [{"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        Collections ->
            {ok, [from_collection(Collection) || Collection <- Collections]}
    end.


to_collection(#uce_meeting{id={Name, Domain},
                           start_date=Start,
                           end_date=End,
                           roster=Roster,
                           metadata=Metadata}) ->
    [{"name", Name},
     {"domain", Domain},
     {"start_date", integer_to_list(Start)},
     {"end_date", integer_to_list(End)},
     {"roster", Roster},
     {"metadata", Metadata}].

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["name", "domain", "start_date", "end_date", "roster", "metadata"]) of
	[Name, Domain, Start, End, Roster, Metadata] ->
	    #uce_meeting{id={Name, Domain},
                     start_date=list_to_integer(Start),
                     end_date=list_to_integer(End),
                     roster=Roster,
                     metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.
