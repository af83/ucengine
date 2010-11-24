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
			    [string, string]},
			   {?MODULE, list,
			    ["uid"],
			    [required],
			    [string]}]},

     #uce_route{module="Organisations",
		method='GET',
		regexp="/org/([^/]+)",
		callbacks=[{?MODULE, get, [], [], []}]},
     
     #uce_route{module="Organisations",
		method='PUT',
		regexp="/org/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, add,
			    ["uid", "metadata"],
			    [none, []],
			    [string, dictionary]}]},
     
     #uce_route{module="Organisations",
		method='POST',
		regexp="/org/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, update,
			    ["uid", "metadata"],
			    [none, []],
			    [string, dictionary]}]}].

add([Name], [Uid, Metadata], _) ->
    case uce_acl:check(Uid, "org", "add", ["", ""], [{"name", Name}]) of
	true ->
	    case uce_org:add(#uce_org{name=Name, metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    uce_event:add(#uce_event{from=Uid,
						 type="internal.org.add",
						 metadata=[{"name", Name}]}),
		    json_helpers:created()
	    end;
	false ->
	    {error, unauthorized}
    end.

update([Name], [Uid, Metadata], _) ->
    case uce_acl:check(Uid, "org", "update", ["", ""], [{"name", Name}]) of
	true ->
	    case uce_org:update(#uce_org{name=Name, metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.

get([Name], [], _) ->
    case uce_org:get(Name) of
	{error, Reason} ->
	    {error, Reason};
	Org ->
	    json_helpers:json(org_helpers:to_json(Org))
    end.

list([], [Uid], _) ->
    case uce_acl:check(Uid, "org", "list", ["", ""], []) of
	true ->
	    case uce_org:list() of
		{error, Reason} ->
		    {error, Reason};
		Orgs ->
		    json_helpers:json({array, [org_helpers:to_json(Org) || Org <- Orgs]})
	    end;
	false ->
	    {error, unauthorized}
    end.
