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

-export([init/0, add/4, list/4, get/4, update/4, leave/4, join/4, roster/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/meeting/all",
                callbacks=[{?MODULE, add,
                            ["uid", "sid", "name", "start", "end", "metadata"],
                            [required, required, required, 0, ?NEVER_ENDING_MEETING, []],
                            [string, string, string, integer, integer, dictionary]}]},
     
     #uce_route{method='GET',
                regexp="/meeting/([^/]+)",
                callbacks=[{?MODULE, list, [], [], []}]},
     
     #uce_route{method='GET',
                regexp="/meeting/all/([^/]+)",
                callbacks=[{?MODULE, get, [], [], []}]},
     
     #uce_route{method='PUT',
                regexp="/meeting/all/([^/]+)",
                callbacks=[{?MODULE, update,
                            ["uid", "sid", "start", "end", "metadata"],
                            [required, required, 0, ?NEVER_ENDING_MEETING, []],
                            [string, string, integer, integer, dictionary]}]},
     
     #uce_route{method='POST',
                regexp="/meeting/all/([^/]+)/roster",
                callbacks=[{?MODULE, join,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},
     
     #uce_route{method='DELETE',
                regexp="/meeting/all/([^/]+)/roster/([^/]+)",
                callbacks=[{?MODULE, leave,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},
     
     #uce_route{method='GET',
                regexp="/meeting/all/([^/]+)/roster",
                callbacks=[{?MODULE, roster,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]}].

add(Domain, [], [Uid, Sid, Name, Start, End, Metadata], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "meeting", "add"),
    {ok, created} = uce_meeting:add(#uce_meeting{id={Name, Domain},
                                                 start_date=Start,
                                                 end_date=End,
                                                 metadata=Metadata}),
    json_helpers:created().

list(Domain, [Status], [], _) ->
    {ok, Meetings} = uce_meeting:list(Domain, Status),
    json_helpers:json({array, [meeting_helpers:to_json(Meeting) || Meeting <- Meetings]}).

get(Domain, [Name], [], _) ->
    {ok, Meeting} = uce_meeting:get({Name, Domain}),
    json_helpers:json(meeting_helpers:to_json(Meeting)).

update(Domain, [Name], [Uid, Sid, Start, End, Metadata], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "meeting", "update", {Name, Domain}),
    {ok, updated} = uce_meeting:update(#uce_meeting{id={Name, Domain},
                                                    start_date=Start,
                                                    end_date=End,
                                                    metadata=Metadata}),
    json_helpers:ok().

join(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "roster", "add", {Name, Domain}),
    {ok, updated} = uce_meeting:join({Name, Domain}, {Uid, Domain}),
%    uce_presence:joinMeeting(utils:domain(Arg), Sid, Id),
    catch uce_event:add(#uce_event{domain=Domain,
                                   type="internal.roster.add",
                                   location={Name, Domain},
                                   from={Uid, Domain}}),
    json_helpers:ok().

%% TODO : Incomplete Sid must be ToSid
leave(Domain, [Name, User], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "roster", "delete", {Name, Domain}),
    {ok, updated} = uce_meeting:leave({Name, Domain}, {User, Domain}),
%    uce_presence:leaveMeeting(Sid, Id),	
    {ok, _} = uce_event:add(#uce_event{domain=Domain,
                                       type="internal.roster.delete",
                                       location={Name, Domain},
                                       from={User, Domain}}),
    json_helpers:ok().

roster(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, Uid, "roster", "list", {Name, Domain}),
    {ok, Roster} = uce_meeting:roster({Name, Domain}),
    json_helpers:json({array, lists:map(fun(Member) ->
                                                {ok, User} = uce_user:get(Member),
                                                user_helpers:to_json(User)
                                        end,
                                        Roster)}).
