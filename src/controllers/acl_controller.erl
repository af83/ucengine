-module(acl_controller).

-export([init/0, check/3, add/3, delete/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="ACL",
		title="Check right",
		desc="Check right",
		path="/user/to/acl/:object/:action/:org/:meeting",
		method='GET',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?([^/]+)?/?",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, check,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary]}]},
     
     #uce_route{module="ACL",
		title="Add right",
		desc="Add right",
		path="/user/to/acl/:object/:action/:org/:meeting",
		method='PUT',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?([^/]+)?/?",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, add,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary]}]},
     
     #uce_route{module="ACL",
		title="Delete right",
		desc="Delete right",
		path="/user/to/acl/:object/:action/:org/:meeting",
		method='DELETE',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?([^/]+)?/?",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, delete,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary]}]}].


check([To, Object, Action], Params, Arg) ->
    ?MODULE:check([To, Object, Action, "", ""], Params, Arg);
check([To, Object, Action, Org], Params, Arg) ->
    ?MODULE:check([To, Object, Action, Org, ""], Params, Arg);
check([To, Object, Action, Org, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "check", [Org, Meeting], [{"user", To},
							      {"action", Action},
							      {"object", Object},
							      {"org", Org},
							      {"meeting", Meeting}] ++ Conditions) of
	true ->
	    case uce_acl:check(To, Object, Action, [Org, Meeting], Conditions) of
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

add([To, Object, Action], Params, Arg) ->
    ?MODULE:add([To, Object, Action, "", ""], Params, Arg);
add([To, Object, Action, Org], Params, Arg) ->
    ?MODULE:add([To, Object, Action, Org, ""], Params, Arg);
add([To, Object, Action, Org, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "add", [Org, Meeting], [{"user", To},
							    {"action", Action},
							    {"object", Object},
							    {"org", Org},
							    {"meeting", Meeting}] ++ Conditions) of
	true ->
	    case uce_acl:add(#uce_acl{uid=To,
				      action=Action,
				      object=Object,
				      location=[Org, Meeting],
				      conditions=Conditions}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:created()
	    end;
	false ->
	    {error, unauthorized}
    end.

delete([To, Object, Action], Params, Arg) ->
    ?MODULE:delete([To, Object, Action, "", ""], Params, Arg);
delete([To, Object, Action, Org], Params, Arg) ->
    ?MODULE:delete([To, Object, Action, Org, ""], Params, Arg);
delete([To, Object, Action, Org, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "delete", [Org, Meeting], [{"user", To},
							       {"action", Action},
							       {"object", Object}] ++ Conditions) of
	true ->
	    case uce_acl:delete(To, Object, Action, [Org, Meeting], Conditions) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.
