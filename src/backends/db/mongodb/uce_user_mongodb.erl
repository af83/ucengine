-module(uce_user_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_user).

-export([add/1,
	 delete/1,
	 update/1,
	 list/0,
	 get/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_user{} = User) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_user", to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

delete(EUid) ->
    case catch emongo:delete(?MONGO_POOL, "uce_user", [{"uid", EUid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, deleted}
    end.    

update(#uce_user{uid=Uid} = User) ->
    case catch emongo:update(?MONGO_POOL, "uce_user", [{"uid", Uid}], to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, updated}
    end.

list() ->
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

get(EUid) ->
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
