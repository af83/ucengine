-module(acl_controller).

-export([init/0, check/3, add/3, delete/3]).

-include("uce.hrl").

init() ->
    {acl, [#uce_route{name="Check right",
			path="/user/{to}/acl/{object}/{action}",
			method='GET',
			regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)",
			callbacks=[{presence_controller, check,
				    ["uid", "sid"],
				    [required, required],
				    [string, string]},
				   {?MODULE, check,
				    ["uid", "conditions"],
				    [required, []],
				    [string, dictionary]}]},
	   
	   #uce_route{name="Add right",
			path="/user/{to}/acl/{object}/{action}",
			method='PUT',
			regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)",
			callbacks=[{presence_controller, check,
				    ["uid", "sid"],
				    [required, required],
				    [string, string]},
				   {?MODULE, add,
				    ["uid", "conditions"],
				    [required, []],
				    [string, dictionary]}]},
	   
	   #uce_route{name="Delete right",
			path="/user/{to}/acl/{object}/{action}",
			method='DELETE',
			regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)",
			callbacks=[{presence_controller, check,
				    ["uid", "sid"],
				    [required, required],
				    [string, string]},
				   {?MODULE, delete,
				    ["uid", "conditions"],
				    [required, []],
				    [string, dictionary]}]}]}.

check([To, Object, Action], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "check", [{"user", To},
						{"action", Action},
						{"object", Object}] ++ Conditions) of
	true ->
	    case uce_acl:check(To, Object, Action, Conditions) of
		{error, Reason} ->
		    {error, Reason};
		true ->
		    json_helpers:true();
		false ->
		    json_helpers:false()
	    end;
	false ->
	    {error, unauthorized}
    end.

add([To, Object, Action], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "add", [{"user", To},
					      {"action", Action},
					      {"object", Object}] ++ Conditions) of
	true ->
	    case uce_acl:add(#uce_acl{uid=To,
					  action=Action,
					  object=Object,
					  conditions=Conditions}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:created()
	    end;
	false ->
	    {error, unauthorized}
    end.

delete([To, Object, Action], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "delete", [{"user", To},
						 {"action", Action},
						 {"object", Object}] ++ Conditions) of
	true ->
	    case uce_acl:delete(To, Object, Action, Conditions) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.
