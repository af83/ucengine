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

list([], [Uid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "user", "list") of
	{ok, true} ->
	    case uce_user:list(utils:domain(Arg)) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Users} ->
		    JSONUsers = [ user_helpers:to_json(User) || User <- Users],
		    json_helpers:json({array, JSONUsers})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

add([Uid], [Auth, Credential, Metadata], Arg) ->
    case uce_user:add(utils:domain(Arg),
                      #uce_user{uid=Uid, auth=Auth, credential=Credential, metadata=Metadata}) of
	{error, Reason} ->
	    {error, Reason};
	{ok, created} ->
	    uce_event:add(utils:domain(Arg), #uce_event{from=Uid, type="internal.user.add"}),
	    json_helpers:created()
    end.

update([To], [Uid, Auth, Credential, Metadata], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "user", "update", [""], [{"user", To}, {"auth", Auth}]) of
	{ok, true} ->
	    case uce_user:update(utils:domain(Arg), To, Auth, Credential, Metadata) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    uce_event:add(utils:domain(Arg), #uce_event{from=To, type="internal.user.update"}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

get([To], [Uid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "user", "get", [""], [{"user", To}]) of
	{ok, true} ->
	    case uce_user:get(utils:domain(Arg), To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, User} ->
		    json_helpers:json(user_helpers:to_json(User))
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

delete([To], [Uid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "user", "delete", [""], [{"user", To}]) of
	{ok, true} ->
	    case uce_user:delete(utils:domain(Arg), To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
