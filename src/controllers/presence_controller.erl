-module(presence_controller).

-export([init/0, delete/3, add/3, check/3]).

-include("uce.hrl").


init() ->
    {presence, [#uce_route{method='PUT',
			     regexp="/presence/([^/]+)/([^/]+)",
			     callbacks=[{?MODULE, add,
					 ["auth", "credential", "metadata"],
					 [required, "", []],
					 [string, string, dictionary]}]},
		
		#uce_route{method='DELETE',
			     regexp="/presence/([^/]+)/([^/]+)/([^/]+)",
			     callbacks=[{presence_controller, check,
					 ["uid", "sid"],
					 [required, required],
					 [string, string]},
					{?MODULE, delete,
					 ["uid", "sid"],
					 [required, required],
					 [string, string]}]}
	       ]}.

add([Org, EUid], [Auth, Credential, Metadata], _)
  when is_list(Auth) ->
    case uce_org:get(Org) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    case uce_acl:check(EUid, "presence", "add", [{"location", [Org]}]) of
		true ->
		    case uce_presence:add(EUid, Org, Auth, Credential, Metadata) of
			ESid when is_list(ESid) ->
			    uce_event:add(#uce_event{location=[Org],
							 from=EUid,
							 type="internal.presence.add",
							 metadata=Metadata}),
			    json_helpers:created(ESid);
			Error ->
			    Error
		    end;
		false ->
		    {error, unauthorized}
	    end
    end.

delete([Org, ToEUid, ToESid], [EUid, ESid], _)
  when is_list(EUid) and is_list(ESid) ->
    case uce_org:get(Org) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    case uce_presence:check(ToESid, ToEUid) of
		true ->
		    case uce_acl:check(EUid, "presence", "delete", [{"user", ToEUid}]) of
			true ->
			    case uce_presence:delete(ESid) of
				ok ->
				    uce_event:add(#uce_event{location=[Org],
								 from=EUid,
								 type="internal.presence.delete"}),
				    json_helpers:json(ok);
				Error ->
				    Error
			    end;
			false ->
			    {error, unauthorized}
		    end;
		false ->
		    {error, not_found}
	    end
    end.

check(_, [EUid, ESid], _) ->
    case uce_presence:check(ESid, EUid) of
	true ->
	    {ok, continue};
	false ->
	    {error, unauthorized}
    end.
