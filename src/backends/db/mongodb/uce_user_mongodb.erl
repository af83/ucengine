-module(uce_user_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 delete/1,
	 update/1,
	 list/0,
	 get/1,
	 from_collection/1,
	 to_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_user{} = User) ->
    case catch emongo:insert(?MONGO_POOL, "uce_user", ?MODULE:to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

delete(EUid) ->
    case catch emongo:delete(?MONGO_POOL, "uce_user", [{"uid", EUid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.    

update(#uce_user{uid=Uid} = User) ->
    case catch emongo:update(?MONGO_POOL, "uce_user", [{"uid", Uid}], ?MODULE:to_collection(User)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

list() ->
    case catch emongo:find(?MONGO_POOL, "uce_user") of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    lists:map(fun(Collection) ->
			      ?MODULE:from_collection(Collection)
		      end,
		      Collections)
    end.

get(EUid) ->
    case emongo:find(?MONGO_POOL, "uce_user", [{"uid", EUid}], [{limit, 1}]) of
	[Collection] ->
	    ?MODULE:from_collection(Collection);
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
