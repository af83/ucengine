-module(user_controller).

-export([init/0, add/3, update/3, get/3, list/3, delete/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Users",
		method='GET',
		regexp="/user",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, list,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Users",
		method='PUT',
		regexp="/user/([^/]+)",
		callbacks=[{?MODULE, add,
			    ["auth", "credential", "metadata"],
			    [required, required, []],
			    [string, string, dictionary],
			    [any, any, any]}]},
     
     #uce_route{module="Users",
		method='POST',
		regexp="/user/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, update,
			    ["uid", "auth", "credential", "metadata"],
			    [required, required, required, []],
			    [string, string, string, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Users",
		method='GET',
		regexp="/user/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, get,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Users",
		method='DELETE',
		regexp="/user/([^/]+)",
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

list([], [Uid], _) ->
    case uce_acl:check(Uid, "user", "list") of
	{ok, true} ->
	    case uce_user:list() of
		{error, Reason} ->
		    {error, Reason};
		{ok, Users} ->
		    JSONUsers = [ user_helpers:to_json(User) || User <- Users],
		    json_helpers:json({array, JSONUsers})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

add([Uid], [Auth, Credential, Metadata], _) ->
    case uce_user:add(#uce_user{uid=Uid, auth=Auth, credential=Credential, metadata=Metadata}) of
	{error, Reason} ->
	    {error, Reason};
	{ok, created} ->
	    uce_event:add(#uce_event{from=Uid, type="internal.user.add"}),
	    json_helpers:created()
    end.

update([To], [Uid, Auth, Credential, Metadata], _) ->
    case uce_acl:check(Uid, "user", "update", [""], [{"user", To}, {"auth", Auth}]) of
	{ok, true} ->
	    case uce_user:update(To, Auth, Credential, Metadata) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    uce_event:add(#uce_event{from=To, type="internal.user.update"}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

get([To], [Uid], _) ->
    case uce_acl:check(Uid, "user", "get", [""], [{"user", To}]) of
	{ok, true} ->
	    case uce_user:get(To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, User} ->
		    json_helpers:json(user_helpers:to_json(User))
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

delete([To], [Uid], _) ->
    case uce_acl:check(Uid, "user", "delete", [""], [{"user", To}]) of
	{ok, true} ->
	    case uce_user:delete(To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
