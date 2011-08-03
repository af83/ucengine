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

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_meeting{}) -> {ok, created}
%% @doc Insert given record #uce_meeting{} in uce_meeting mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_meeting{} = Meeting) ->
    mongodb_helpers:ok(emongo:insert_sync(Domain, "uce_meeting", to_collection(Domain, Meeting))),
    {ok, created}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, Name::list) -> {ok, deleted}
%% @doc Delete record
%% @end
%%--------------------------------------------------------------------
delete(Domain, Name) ->
    mongodb_helpers:ok(emongo:delete_sync(Domain, "uce_meeting", [{"name", Name}, {"domain", Domain}])),
    {ok, deleted}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, Name::list) -> {ok, #uce_meeting{}} | {error, not_found}
%% @doc Get record uce_meeting which correspond to the given name and domain
%% @end
%%--------------------------------------------------------------------
get(Domain, Name) ->
    case emongo:find_one(Domain, "uce_meeting",
                         [{"name", Name}, {"domain", Domain}]) of
        [Record] ->
            {ok, from_collection(Record)};
        [] ->
            throw({error, not_found})
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_meeting{}) -> {ok, updated}
%% @doc update #uce_meeting record
%% @end
%%--------------------------------------------------------------------
update(Domain, #uce_meeting{id=Name} = Meeting) ->
    mongodb_helpers:updated(emongo:update_sync(Domain, "uce_meeting",
                                               [{"name", Name}, {"domain", Domain}],
                                               to_collection(Domain, Meeting), false)),
    {ok, updated}.

%%--------------------------------------------------------------------
%% @spec (Domain::list) -> {ok, [#uce_meeting{}, #uce_meeting{}, ..] = Meetings::list} | {error, bad_parameters}
%% @doc List all record #uce_meeting for the given domain
%% @end
%%--------------------------------------------------------------------
list(Domain) ->
    Collections = emongo:find_all(Domain, "uce_meeting", [{"domain", Domain}]),
    {ok, [from_collection(Collection) || Collection <- Collections]}.


%%--------------------------------------------------------------------
%% @spec (#uce_meeting{}) -> [{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list
%% @doc Convert #uce_meeting{} record to valid collection
%% @end
%%--------------------------------------------------------------------
to_collection(Domain, #uce_meeting{id=Name,
                                   start_date=Start,
                                   end_date=End,
                                   roster=Roster,
                                   metadata=Metadata}) ->
    [{"name", Name},
     {"domain", Domain},
     {"start_date", integer_to_list(Start)},
     {"end_date", integer_to_list(End)},
     {"roster", {array, Roster}},
     {"metadata", mongodb_helpers:to_bson(Metadata)}].

%%--------------------------------------------------------------------
%% @spec ([{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list) -> #uce_meeting{} | {error, bad_parameters}
%% @doc Convert collection returned by mongodb to valid record #uce_meeting{}
%% @end
%%--------------------------------------------------------------------
from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["name", "domain", "start_date", "end_date", "roster", "metadata"]) of
        [Name, _Domain, Start, End, {array, Roster}, Metadata] ->
            #uce_meeting{id=Name,
                     start_date=list_to_integer(Start),
                     end_date=list_to_integer(End),
                     roster=Roster,
                     metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.
