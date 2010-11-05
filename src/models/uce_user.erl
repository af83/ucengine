-module(uce_user).

-author('tbomandouki@af83.com').

-export([add/4, add/1, delete/1, update/4, update/1, list/0, get/1]).

-include("uce.hrl").
-include("models.hrl").

add(EUid, Auth, Credential, Metadata) ->
    case ?MODULE:get(EUid) of
	{error, not_found} ->
	    ?MODULE:add(#uce_user{uid=EUid,
				    auth=Auth,
				    credential=Credential,
				    metadata=Metadata});
	{error, Reason} ->
	    {error, Reason};
	_Record ->
	    {error, conflict}
    end.

add(#uce_user{} = User) ->
    case ?DBMOD:add(User) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

delete(EUid) ->
    case ?MODULE:get(EUid) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:delete(EUid)
    end.

update(EUid, Auth, Credential, Metadata) ->
    ?MODULE:update(#uce_user{uid=EUid,
			       auth=Auth,
			       credential=Credential,
			       metadata=Metadata}).

update(#uce_user{} = User) ->
    case ?MODULE:get(User#uce_user.uid) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:update(User)
    end.

list() ->
    ?DBMOD:list().

get(EUid) ->
    ?DBMOD:get(EUid).
