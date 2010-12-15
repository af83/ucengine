-module(uce_user).

-author('tbomandouki@af83.com').

-export([add/1, delete/1, update/4, update/1, list/0, get/1, exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_user{} = User) ->
    case ?MODULE:exists(User#uce_user.uid) of
	true ->
	    {error, conflict};
	false ->
	    ?DB_MODULE:add(User)
    end.

delete(Uid) ->
    case ?MODULE:get(Uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Uid)
    end.

update(Uid, Auth, Credential, Metadata) ->
    ?MODULE:update(#uce_user{uid=Uid,
			     auth=Auth,
			     credential=Credential,
			     metadata=Metadata}).

update(#uce_user{} = User) ->
    case ?MODULE:get(User#uce_user.uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(User)
    end.

list() ->
    ?DB_MODULE:list().

get(Uid) ->
    ?DB_MODULE:get(Uid).

exists(Uid) ->
    case ?MODULE:get(Uid) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.
