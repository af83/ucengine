-module(presence_controller).

-export([init/0, delete/3, add/3, check/3]).

-include("uce.hrl").


init() ->
    [#uce_route{module="Presence",
		method='PUT',
		regexp="/presence/([^/]+)",
		callbacks=[{?MODULE, add,
			    ["auth", "credential", "metadata"],
			    [required, required, []],
			    [string, string, dictionary],
			    [any, any, any]}]},
     
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

add([Uid], [Auth, Credential, Metadata], _) ->
    case uce_acl:check(Uid, "presence", "add", ["", ""], []) of
	true ->
	    case uce_user:get(Uid) of
		{error, Reason} ->
		    {error, Reason};
		#uce_user{} = User ->
		    case {User#uce_user.auth, User#uce_user.credential} of
			{Auth, Credential} ->
			    case uce_presence:add(#uce_presence{uid=Uid,
								auth=Auth,
								metadata=Metadata}) of
				{error, Reason} ->
				    {error, Reason};
				Sid ->
				    uce_event:add(#uce_event{location=["", ""],
							     from=Uid,
							     type="internal.presence.add",
							     metadata=Metadata}),
				    json_helpers:created(Sid)
			    end;
			_ ->
			    {error, bad_credentials}
		    end
	    end;
	false ->
	    {error, unauthorized}
    end.

delete([ToUid, ToSid], [Uid], _) ->
    case uce_acl:check(Uid, "presence", "delete", ["", ""], [{"user", ToUid}]) of
	true ->
	    case uce_presence:delete(ToSid) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    uce_event:add(#uce_event{location=["", ""],
					     from=Uid,
					     type="internal.presence.delete"}),
		    json_helpers:json(ok)
	    end;
	false ->
	    {error, unauthorized}
    end.

check(_, [Uid, Sid], _) ->
    case uce_presence:get(Sid) of
	{error, Reason} ->
	    {error, Reason};
	Presence ->
	    case Presence#uce_presence.uid of
		Uid ->
		    {ok, continue};
		_ ->
		    {error, unauthorized}
	    end
    end.
