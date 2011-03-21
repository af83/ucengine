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
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"name", required, string},
                           {"start", 0, integer},
                           {"end", ?NEVER_ENDING_MEETING, integer},
                           {"metadata", [], dictionary}]}},
     
     #uce_route{method='GET',
                regexp="/meeting/([^/]+)",
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     
     #uce_route{method='GET',
                regexp="/meeting/all/([^/]+)",
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},
     
     #uce_route{method='PUT',
                regexp="/meeting/all/([^/]+)",
                callback={?MODULE, update,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"start", 0, integer},
                           {"end", ?NEVER_ENDING_MEETING, integer},
                           {"metadata", [], dictionary}]}},
     
     #uce_route{method='POST',
                regexp="/meeting/all/([^/]+)/roster",
                callback={?MODULE, join,
                          [{"uid", required, string},
                           {"sid", required, string}]}},
     
     #uce_route{method='DELETE',
                regexp="/meeting/all/([^/]+)/roster/([^/]+)",
                callback={?MODULE, leave,
                          [{"uid", required, string},
                           {"sid", required, string}]}},
     
     #uce_route{method='GET',
                regexp="/meeting/all/([^/]+)/roster",
                callback={?MODULE, roster,
                          [{"uid", required, string},
                           {"sid", required, string}]}}].

add(Domain, [], [Uid, Sid, Name, Start, End, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "meeting", "add"),
    {ok, created} = uce_meeting:add(Domain,
                                    #uce_meeting{id={Name, Domain},
                                                 start_date=Start,
                                                 end_date=End,
                                                 metadata=Metadata}),
    json_helpers:created(Domain).

list(Domain, [Status], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "meeting", "list"),
    {ok, Meetings} = uce_meeting:list(Domain, Status),
    json_helpers:json(Domain, {array, [meeting_helpers:to_json(Meeting) || Meeting <- Meetings]}).

get(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "meeting", "get"),
    {ok, Meeting} = uce_meeting:get(Domain, {Name, Domain}),
    json_helpers:json(Domain, meeting_helpers:to_json(Meeting)).

update(Domain, [Name], [Uid, Sid, Start, End, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Name, Domain}, "meeting", "update"),
    {ok, updated} = uce_meeting:update(Domain,
                                       #uce_meeting{id={Name, Domain},
                                                    start_date=Start,
                                                    end_date=End,
                                                    metadata=Metadata}),
    json_helpers:ok(Domain).

join(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Name, Domain}, "roster", "add"),
    {ok, updated} = uce_meeting:join(Domain, {Name, Domain}, {Uid, Domain}),
    uce_presence:join(Domain, {Sid, Domain}, {Name, Domain}),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id={none, Domain},
                                       type="internal.roster.add",
                                       location={Name, Domain},
                                       from={Uid, Domain}}),
    json_helpers:ok(Domain).

%% TODO : Incomplete Sid must be ToSid
leave(Domain, [Name, User], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Name, Domain}, "roster", "delete"),
    {ok, updated} = uce_meeting:leave(Domain, {Name, Domain}, {User, Domain}),
    uce_presence:leave(Domain, {Sid, Domain}, {Name, Domain}),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id={none, Domain},
                                       type="internal.roster.delete",
                                       location={Name, Domain},
                                       from={User, Domain}}),
    json_helpers:ok(Domain).

roster(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Name, Domain}, "roster", "list"),
    {ok, Roster} = uce_meeting:roster(Domain, {Name, Domain}),
    json_helpers:json(Domain,
                      {array, lists:map(fun(Member) ->
                                                {ok, User} = uce_user:get(Domain, Member),
                                                user_helpers:to_json(User)
                                        end,
                                        Roster)}).
