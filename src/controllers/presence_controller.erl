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

-export([init/0, delete/3, add/3, check/3, timeout/0, clean/0]).

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
		callbacks=[{?MODULE, check,
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
	    F = fun(M) ->
                   uce_event:add(#uce_event{from=ToUid, type="internal.roster.delete", location=[M]}), 
		   uce_meeting:leave([M], ToUid)
	        end, 
            case uce_presence:get(ToSid) of
                {ok, Presence} ->
	             [ F(Meeting) || Meeting <- Presence#uce_presence.meetings ];
                 _ -> nothing
            end,
	    case uce_presence:delete(ToSid) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
                    uce_event:add(#uce_event{location=[""], from=ToUid, type="internal.presence.delete"}),
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
            uce_presence:update(Presence#uce_presence{last_activity=utils:now()}),
		    {ok, continue};
		_ ->
		    {error, unauthorized}
	    end
    end.

clean() ->
    ?MODULE:timeout(),
    case config:get(presence_timeout) of
        Value when is_integer(Value) ->
            timer:sleep(Value * 1000);
        _ ->
            timer:sleep(?DEFAULT_TIME_INTERVAL)
    end,
    ?MODULE:clean().

timeout() ->
    case uce_presence:all() of
        {ok, Presences} ->
            delete_expired_presence(Presences);
        _ ->
            nothing
    end.

delete_expired_presence([#uce_presence{sid=Sid, uid=Uid, last_activity=Last, auth=Auth, meetings=Meetings} | TlPresences]) ->
    ?DEBUG("Timeout: ~p~n", [?SESSION_TIMEOUT]),
    Timeout = Last + ?SESSION_TIMEOUT,
    Now = utils:now(),
    if
        Now >= Timeout , Auth == "password", Uid /= "root" ->
            F = fun(M) ->
                   uce_event:add(#uce_event{from=Uid, type="internal.roster.delete", location=[M]}), 
                   uce_meeting:leave([M], Uid)
                end,
            [ F(Meeting) || Meeting <- Meetings ],
            uce_event:add(#uce_event{ location=[""],
                                      type="internal.presence.delete",
                                      from=Uid}),
            uce_presence:delete(Sid);
        true ->
            nothing
    end,
    delete_expired_presence(TlPresences);
delete_expired_presence([]) ->
    ok.

