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
-module(presence_controller).

-export([init/0, delete/3, add/3, check/3]).

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
