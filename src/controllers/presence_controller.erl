-module(presence_controller).

-export([init/0, delete/3, add/3, check/3, timeout/0]).

-include("uce.hrl").
-include("uce_auth.hrl").

init() ->
    [#uce_route{module="Presence",
		method='PUT',
		regexp="/presence/([^/]+)",
		callbacks=[{?MODULE, add,
			    ["credential", "metadata"],
			    [required, []],
			    [string, dictionary],
			    [any, any]}]},
     
     #uce_route{module="Presence",
		method='DELETE',
		regexp="/presence/([^/]+)/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, delete,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]}].

add([Uid], [Credential, Metadata], _) ->
    case uce_acl:check(Uid, "presence", "add") of
	{ok, true} ->
	    case uce_user:get(Uid) of
		{error, Reason} ->
		    {error, Reason};
		{ok, #uce_user{} = User} ->
		    case catch ?AUTH_MODULE(User#uce_user.auth):check(User, Credential) of
			true ->
			    case uce_presence:add(#uce_presence{uid=Uid,
								auth=User#uce_user.auth,
								metadata=Metadata}) of
				{error, Reason} ->
				    {error, Reason};
				{ok, Sid} ->
				    uce_event:add(#uce_event{location=[""],
							     from=Uid,
							     type="internal.presence.add",
							     metadata=Metadata}),
				    json_helpers:created(Sid)
			    end;
			false ->
			    {error, bad_credentials};
			_ ->
			    {error, bad_credentials}
		    end
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

delete([ToUid, ToSid], [Uid], _) ->
    case uce_acl:check(Uid, "presence", "delete", [""], [{"user", ToUid}]) of
	{ok, true} ->
	    case uce_presence:delete(ToSid) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    uce_event:add(#uce_event{location=[""],
					     from=Uid,
					     type="internal.presence.delete"}),
		    json_helpers:json(ok)
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

check(_, [Uid, Sid], _) ->
    case uce_presence:get(Sid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Presence} ->
	    case Presence#uce_presence.uid of
		Uid ->
		    {ok, continue};
		_ ->
		    {error, unauthorized}
	    end
    end.

timeout() ->
    case uce_presence:all() of
        {ok, Presences} ->
            delete_expired_presence(Presences);
        _ ->
            nothing
    end.

delete_expired_presence([#uce_presence{sid=Sid, uid=Uid, last_activity=Last, auth=Auth}=HdPresence | TlPresences]) ->
    ?DEBUG("Timeout: ~p~n", [?SESSION_TIMEOUT]),
    Timeout = Last + ?SESSION_TIMEOUT,
    Now = utils:now(),
    if
        Now >= Timeout , Auth == "password", Uid /= "root" ->
            event_controller:add([], [Uid, ?PRESENCE_EXPIRED_EVENT, [], [], []], []),
            uce_presence:delete(Sid);
        true ->
            nothing
    end,
    delete_expired_presence(TlPresences);
delete_expired_presence([]) ->
    ok.

