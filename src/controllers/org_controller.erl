-module(org_controller).

-export([init/0, add/3, update/3, list/3, get/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Organisations",
		method='GET',
		regexp="/org/",
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

     #uce_route{module="Organisations",
		method='GET',
		regexp="/org/([^/]+)",
		callbacks=[{?MODULE, get, [], [], [], []}]},
     
     #uce_route{module="Organisations",
		method='PUT',
		regexp="/org/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "metadata"],
			    [none, []],
			    [string, dictionary],
			    [user, any]}]},
     
     #uce_route{module="Organisations",
		method='POST',
		regexp="/org/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, update,
			    ["uid", "metadata"],
			    [none, []],
			    [string, dictionary],
			    [user, any]}]}].

add([Name], [Uid, Metadata], _) ->
    case uce_acl:check(Uid, "org", "add", ["", ""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_org:add(#uce_org{name=Name, metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    uce_event:add(#uce_event{from=Uid,
					     type="internal.org.add",
					     metadata=[{"name", Name}]}),
		    json_helpers:created()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

update([Name], [Uid, Metadata], _) ->
    case uce_acl:check(Uid, "org", "update", ["", ""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_org:update(#uce_org{name=Name, metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

get([Name], [], _) ->
    case uce_org:get(Name) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Org} ->
	    json_helpers:json(org_helpers:to_json(Org))
    end.

list([], [Uid], _) ->
    case uce_acl:check(Uid, "org", "list", ["", ""], []) of
	{ok, true} ->
	    case uce_org:list() of
		{error, Reason} ->
		    {error, Reason};
		{ok, Orgs} ->
		    json_helpers:json({array, [org_helpers:to_json(Org) || Org <- Orgs]})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
