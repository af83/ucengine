%%
%%  U.C.Engine - Unified Colloboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(acl_controller).

-export([init/0, check/3, add/3, delete/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="ACL",
		title="Check right",
		desc="Check right",
		path="/user/:to/acl/:object/:action/:meeting",
		method='GET',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
		types=[user, any, any, meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, check,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary],
			    [user, any]}]},
     
     #uce_route{module="ACL",
		title="Add right",
		desc="Add right",
		path="/user/:to/acl/:object/:action/:meeting",
		method='PUT',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
		types=[user, any, any, meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary],
			    [user, any]}]},
     
     #uce_route{module="ACL",
		title="Delete right",
		desc="Delete right",
		path="/user/:to/acl/:object/:action/:meeting",
		method='DELETE',
		regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
		types=[user, any, any, meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, delete,
			    ["uid", "conditions"],
			    [required, []],
			    [string, dictionary],
			    [user, any]}]}].


check([To, Object, Action], Params, Arg) ->
    check([To, Object, Action, ""], Params, Arg);
check([To, Object, Action, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "check", [Meeting], [{"user", To},
                                                         {"action", Action},
                                                         {"object", Object},
                                                         {"meeting", Meeting}] ++ Conditions) of
	{ok, true} ->
	    case uce_acl:check(To, Object, Action, [Meeting], Conditions) of
		{error, Reason} ->
		    {error, Reason};
		{ok, true} ->
		    json_helpers:true();
		{ok, false} ->
		    json_helpers:false()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

add([To, Object, Action], Params, Arg) ->
    add([To, Object, Action, ""], Params, Arg);
add([To, Object, Action, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "add", [Meeting], [{"user", To},
                                                       {"action", Action},
                                                       {"object", Object},
                                                       {"meeting", Meeting}] ++ Conditions) of
	{ok, true} ->
	    case uce_acl:add(#uce_acl{uid=To,
				      action=Action,
				      object=Object,
				      location=[Meeting],
				      conditions=Conditions}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    json_helpers:created()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

delete([To, Object, Action], Params, Arg) ->
    delete([To, Object, Action, "", ""], Params, Arg);
delete([To, Object, Action, Meeting], [EUid, Conditions], _) ->
    case uce_acl:check(EUid, "acl", "delete", [Meeting], [{"user", To},
                                                          {"action", Action},
                                                          {"object", Object}] ++ Conditions) of
	{ok, true} ->
	    case uce_acl:delete(To, Object, Action, [Meeting], Conditions) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
