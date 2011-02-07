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

add(_Domain, #uce_user{} = User) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_user", to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

delete(_Domain, EUid) ->
    case catch emongo:delete(?MONGO_POOL, "uce_user", [{"uid", EUid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, deleted}
    end.    

update(_Domain, #uce_user{uid=Uid} = User) ->
    case catch emongo:update(?MONGO_POOL, "uce_user", [{"uid", Uid}], to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, updated}
    end.

list(_Domain) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_user") of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    Users = lists:map(fun(Collection) ->
				      from_collection(Collection)
			      end,
			      Collections),
	    {ok, Users}
    end.

get(_Domain, EUid) ->
    case emongo:find_one(?MONGO_POOL, "uce_user", [{"uid", EUid}]) of
	[Collection] ->
	    {ok, from_collection(Collection)};
	_ ->
	    {error, not_found}
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["uid", "auth", "credential", "metadata"]) of
	[EUid, Auth, Credential, Metadata] ->
	    #uce_user{uid=EUid,
		      auth=Auth,
		      credential=Credential,
		      metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_user{} = User) ->
    [{"uid", User#uce_user.uid},
     {"auth", User#uce_user.auth},
     {"credential", User#uce_user.credential},
     {"metadata", User#uce_user.metadata}].
