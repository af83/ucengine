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

-export([add/1,
         delete/1,
         update/1,
         list/1,
         get/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_user{} = User) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_user", to_collection(User)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, created}
    end.

delete({Name, Domain}) ->
    case catch emongo:delete(?MONGO_POOL, "uce_user", [{"name", Name},
                                                       {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, deleted}
    end.    

update(#uce_user{id={Name, Domain}} = User) ->
    case catch emongo:update(?MONGO_POOL, "uce_user", [{"name", Name},
                                                       {"domain", Domain}],
                             to_collection(User), false) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, updated}
    end.

list(Domain) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_user", [{"domain", Domain}]) of
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

get({Name, Domain}) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_user", [{"name", Name},
                                                         {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Collection] ->
            {ok, from_collection(Collection)};
        [] ->
            throw({error, not_found})
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["name", "domain", "auth", "credential", "metadata"]) of
        [Name, Domain, Auth, Credential, Metadata] ->
            #uce_user{id={Name, Domain},
                      auth=Auth,
                      credential=Credential,
                      metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.

to_collection(#uce_user{id={Name, Domain},
                        auth=Auth,
                        credential=Credential,
                        metadata=Metadata}) ->
    [{"name", Name},
     {"domain", Domain},
     {"auth", Auth},
     {"credential", Credential},
     {"metadata", Metadata}].
