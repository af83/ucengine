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
         get/2]).

-include("uce.hrl").
-include("mongodb.hrl").

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_user{}) -> {ok, created} | {error, bad_parameters}
%% @doc Insert given record #uce_user{} in uce_user mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_user{} = User) ->
    case catch emongo:insert_sync(Domain, "uce_user", to_collection(User)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, created}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, {Id::list, Domain::list}) -> {ok, deleted} | {error, bad_parameters}
%% @doc Delete uce_user record which corresponds to given id and domain
%% @end
%%--------------------------------------------------------------------
delete(Domain, {Id, Domain}) ->
    case catch emongo:delete_sync(Domain, "uce_user", [{"id", Id},
                                                       {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_user{}) -> {ok, updated} | {error, bad_parameters}
%% @doc Update given record #uce_user{} in uce_user mongodb table
%% @end
%%--------------------------------------------------------------------
update(Domain, #uce_user{id={Id, UDomain}} = User) ->
    case catch emongo:update_sync(Domain, "uce_user", [{"id", Id},
                                                       {"domain", UDomain}],
                                  to_collection(User), false) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, updated}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list) -> {ok, [#uce_user{}, #uce_user{}, ...] = Users::list} | {error, bad_parameters}
%% @doc List all uce_user record for given domain
%% @end
%%--------------------------------------------------------------------
list(Domain) ->
    case catch emongo:find_all(Domain, "uce_user", [{"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        Collections ->
            Users = lists:map(fun(Collection) ->
                                      from_collection(Collection)
                              end,
                              Collections),
            {ok, Users}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, {Id::list, Domain::list}) -> {ok, #uce_user{}} | {error, not_found} | {error, bad_parameters}
%% @doc Get uce_user record for given name or id and domain
%% @end
%%--------------------------------------------------------------------
get(Domain, Name) when is_list(Name) ->
    case catch emongo:find_one(Domain, "uce_user", [{"name", Name},
                                                    {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Collection] ->
            {ok, from_collection(Collection)};
        [] ->
            throw({error, not_found})
    end;
get(Domain, {UId, UDomain}) ->
    case catch emongo:find_one(Domain, "uce_user", [{"id", UId},
                                                    {"domain", UDomain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
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
        [Id, Name, Domain, Auth, Credential, Metadata, Roles] ->
            #uce_user{id={Id, Domain},
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
to_collection(#uce_user{id={Id, Domain},
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
