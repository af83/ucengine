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
-module(meeting_controller).

-export([init/0, add/3, list/3, get/3, update/3, leave/3, join/3, roster/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/([^/]+)",
		callbacks=[{?MODULE, list, [], [], [], []}]},
     
     #uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/all/([^/]+)",
		callbacks=[{?MODULE, get, [], [], [], []}]},
		   
     #uce_route{module="Meetings",
		method='PUT',
		regexp="/meeting/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Meetings",
		method='POST',
		regexp="/meeting/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, update,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Roster",
		method='PUT',
		regexp="/meeting/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, join,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]}]},
     
     #uce_route{module="Roster",
		method='DELETE',
		regexp="/meeting/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, leave,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]}]},
     
     #uce_route{module="Roster",
		method='GET',
		regexp="/meeting/all/([^/]+)/roster",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, roster,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]}].

add([Name], [Uid, Start, End, Metadata], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "meeting", "add", [""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_meeting:add(#uce_meeting{id=[Name],
					      start_date=Start,
					      end_date=End,
					      metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    json_helpers:created()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

update([Name], [Uid, Start, End, Metadata], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "meeting", "update", [""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_meeting:update(utils:domain(Arg), #uce_meeting{id=[Name],
						 start_date=Start,
						 end_date=End,
						 metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

list([Status], [], Arg) ->
    case uce_meeting:list(utils:domain(Arg), Status) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Meetings} ->
	    json_helpers:json({array, [meeting_helpers:to_json(Meeting) || Meeting <- Meetings]})
    end.

get(Location, [], Arg) ->
    case uce_meeting:get(utils:domain(Arg), Location) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Meeting} ->
	    json_helpers:json(meeting_helpers:to_json(Meeting))
    end.

join([Meeting, To], [Uid, Sid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "roster", "add", [Meeting]) of
	{ok, true} ->
	    case uce_meeting:join(utils:domain(Arg), [Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
                    uce_presence:joinMeeting(utils:domain(Arg), Sid, Meeting),	
		    uce_event:add(utils:domain(Arg), #uce_event{type="internal.roster.add",
					     location=[Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

%% TODO : Incomplete Sid must be ToSid
leave([Meeting, To], [Uid, Sid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "roster", "delete", [Meeting]) of
	{ok, true} ->
	    case uce_meeting:leave(utils:domain(Arg), [Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
                    uce_presence:leaveMeeting(utils:domain(Arg), Sid, Meeting),	
		    uce_event:add(utils:domain(Arg),# uce_event{type="internal.roster.delete",
					     location=[Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

roster(Location, [Uid], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "roster", "list", Location) of
	{ok, true} ->
	    case uce_meeting:roster(utils:domain(Arg), Location) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Roster} ->
		    FullRoster =
			lists:map(fun(MemberUid) ->
					  case uce_user:get(utils:domain(Arg), MemberUid) of
					      {ok, User} ->
						  {struct, [{uid, User#uce_user.uid},
							    {auth, User#uce_user.auth},
							    {metadata, {struct, User#uce_user.metadata}}]};
					      {error, _} ->
						  []
					  end
				  end,
				  Roster),
		    json_helpers:json({array, FullRoster})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
