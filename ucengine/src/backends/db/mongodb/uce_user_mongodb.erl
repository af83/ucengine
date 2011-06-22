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
-module(uce_user_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_user).

-export([add/2,
         delete/2,
         update/2,
         list/1,
         get/2,
         get_by_name/2]).

-include("uce.hrl").
-include("mongodb.hrl").

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_user{}) -> {ok, created}
%% @doc Insert given record #uce_user{} in uce_user mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_user{} = User) ->
    mongodb_helpers:ok(emongo:insert_sync(Domain, "uce_user", to_collection(Domain, User))),
    {ok, created}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, Id::list) -> {ok, deleted}
%% @doc Delete uce_user record which corresponds to given id and domain
%% @end
%%--------------------------------------------------------------------
delete(Domain, Id) ->
    mongodb_helpers:ok(emongo:delete_sync(Domain, "uce_user", [{"id", Id},
                                                               {"domain", Domain}])),
    {ok, deleted}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_user{}) -> {ok, updated}
%% @doc Update given record #uce_user{} in uce_user mongodb table
%% @end
%%--------------------------------------------------------------------
update(Domain, #uce_user{id=Id} = User) ->
    mongodb_helpers:updated(emongo:update_sync(Domain, "uce_user", [{"id", Id},
                                                                    {"domain", Domain}],
                                               to_collection(Domain, User), false)),
    {ok, updated}.

%%--------------------------------------------------------------------
%% @spec (Domain::list) -> {ok, [#uce_user{}, #uce_user{}, ...] = Users::list} | {error, bad_parameters}
%% @doc List all uce_user record for given domain
%% @end
%%--------------------------------------------------------------------
list(Domain) ->
    Collections = emongo:find_all(Domain, "uce_user", [{"domain", Domain}]),
    Users = lists:map(fun(Collection) ->
                              from_collection(Collection)
                      end,
                      Collections),
    {ok, Users}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, Id::list) -> {ok, #uce_user{}} | {error, not_found} | {error, bad_parameters}
%% @doc Get uce_user record for given name or id and domain
%% @end
%%--------------------------------------------------------------------
get_by_name(Domain, Name) ->
    case emongo:find_one(Domain, "uce_user", [{"name", Name},
                                              {"domain", Domain}]) of
        [Collection] ->
            {ok, from_collection(Collection)};
        [] ->
            throw({error, not_found})
    end.

get(Domain, UId) ->
    case emongo:find_one(Domain, "uce_user", [{"id", UId},
                                              {"domain", Domain}]) of
        [Collection] ->
            {ok, from_collection(Collection)};
        [] ->
            throw({error, not_found})
    end.

%%--------------------------------------------------------------------
%% @spec ([{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list) -> #uce_user{} | {error, bad_parameters}
%% @doc Convert collection returned by mongodb to valid record #uce_user{}
%% @end
%%--------------------------------------------------------------------
from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["id", "name", "domain", "auth", "credential", "metadata", "roles"]) of
        [Id, Name, _Domain, Auth, Credential, Metadata, Roles] ->
            #uce_user{id=Id,
                      name=Name,
                      auth=Auth,
                      credential=Credential,
                      metadata=Metadata,
                      roles=[{Role, Location} || [Role, Location] <- Roles]};
        _ ->
            throw({error, bad_parameters})
    end.

%%--------------------------------------------------------------------
%% @spec (#uce_user{}) -> [{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list
%% @doc Convert #uce_user{} record to valid collection
%% @end
%%--------------------------------------------------------------------
to_collection(Domain, #uce_user{id=Id,
                                name=Name,
                                auth=Auth,
                                credential=Credential,
                                metadata=Metadata,
                                roles=Roles}) ->
    [{"id", Id},
     {"name", Name},
     {"domain", Domain},
     {"auth", Auth},
     {"credential", Credential},
     {"metadata", Metadata},
     {"roles", [[Role, Location] || {Role, Location} <- Roles]}].
