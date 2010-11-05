-module(uce_presence).

-author('tbomandouki@af83.com').

-export([
	 auth/3,

	 add/5,
	 add/1,

	 list/1,
	 list/2,

	 get/1,

	 delete/1,

	 update/1,

	 check/2
	]).

-include("uce.hrl").
-include("models.hrl").

auth(EUid, "phonecall", Credential) ->
    ?MODULE:auth(EUid, anonymous, Credential);
auth(EUid, "anonymous", _) ->
    #uce_presence{uid=EUid};
auth(EUid, Auth, Credential) ->
    case uce_user:get(EUid) of
	{error, Reason} ->
    {error, Reason};
	User when is_record(User, uce_user) ->
	    case {User#uce_user.auth, User#uce_user.credential} of
		{Auth, Credential} ->
		    #uce_presence{uid=EUid};
		_ ->
		    {error, bad_credentials}
	    end
    end.

add(EUid, Org, Auth, Credential, Metadata) ->
    case ?MODULE:auth(EUid, Auth, Credential) of
	#uce_presence{} = Session ->
	    ?MODULE:add(Session#uce_presence{org=Org,
					      metadata=Metadata});
	{error, Reason} ->
	    {error, Reason}
    end.

add(#uce_presence{}=Session) ->
    ESid = case Session#uce_presence.sid of
	       [] ->
		   utils:random();
	       _ ->
		   Session#uce_presence.sid
	   end,
    LastActivity = case Session#uce_presence.last_activity of
		       never ->
			   never;
		       _ ->
			   utils:now()
		   end,
    ?DBMOD:add(Session#uce_presence{sid=ESid, last_activity=LastActivity}).

list(EUid) ->
    ?MODULE:list(EUid, '_').

list(EUid, Org) ->
    ?DBMOD:list(EUid, Org).

get(ESid) ->
    ?DBMOD:get(ESid).

delete(ESid) when is_list(ESid) ->
    case ?MODULE:get(ESid) of
	Session when is_record(Session, uce_presence) ->
	    ?MODULE:delete(Session);
	{error, Reason} ->
	    {error, Reason}
    end;
delete(#uce_presence{}=Session) ->
    ?DBMOD:delete(Session).

update(#uce_presence{}=Session) ->
    case ?MODULE:get(Session#uce_presence.sid) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:update(Session)
    end.

check(ESid, EUid) ->
    case ?MODULE:get(ESid) of
	Session when is_record(Session, uce_presence) ->
	    case Session#uce_presence.uid of
	       EUid ->
		    true;
	       _ ->
		    false
	    end;
	{error, _Reason} ->
	    false
    end.
