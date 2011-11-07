%%
%%  U.C.Engine - Unified Collaboration Engine
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

-export([init/0, add/5, list/5, get/5, update/5, leave/5, join/5, roster/5]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                path=["meeting"],
                middlewares = [auth,
                               {params, [{"name", required, string},
                                         {"metadata", [], dictionary}]}],
                callback={?MODULE, add}},

     #uce_route{method='GET',
                path=["meeting"],
                middlewares = [auth],
                callback={?MODULE, list}},


     #uce_route{method='GET',
                path=["meeting", meeting],
                middlewares = [auth],
                callback={?MODULE, get}},

     #uce_route{method='PUT',
                path=["meeting", meeting],
                middlewares = [auth,
                               {params, [{"metadata", [], dictionary}]}],
                callback={?MODULE, update}},

     #uce_route{method='POST',
                path=["meeting", meeting, "roster"],
                middlewares = [auth,
                               {params, [{"metadata", [], dictionary}]}],
                callback={?MODULE, join}},

     #uce_route{method='DELETE',
                path=["meeting", meeting, "roster", uid],
                middlewares = [auth],
                callback={?MODULE, leave}},

     #uce_route{method='GET',
                path=["meeting", meeting, "roster"],
                middlewares = [auth],
                callback={?MODULE, roster}}].

add(Domain, [], [Uid, _Sid, Name, Metadata], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "meeting", "add"),
    {ok, created} = uce_meeting:add(Domain,
                                    #uce_meeting{id=Name,
                                                 metadata=json_helpers:to_struct(Metadata)}),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location=Name,
                                               type="internal.meeting.add"}),
    json_helpers:created(Response).

list(Domain, [], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "meeting", "list"),
    {ok, Meetings} = uce_meeting:list(Domain),
    json_helpers:json(Response, Domain, Meetings).

get(Domain, [{meeting, Name}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "meeting", "get"),
    {ok, Meeting} = uce_meeting:get(Domain, Name),
    json_helpers:json(Response, Domain, Meeting).

update(Domain, [{meeting, Name}], [Uid, _Sid, Metadata], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Name, "meeting", "update"),
    {ok, Meeting} = uce_meeting:get(Domain, Name),
    {ok, updated} = uce_meeting:update(Domain,
                                       #uce_meeting{id=Name,
                                                    roster=Meeting#uce_meeting.roster,
                                                    metadata=json_helpers:to_struct(Metadata)}),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location=Name,
                                               type="internal.meeting.update"}),
    json_helpers:ok(Response).

join(Domain, [{meeting, Name}], [Uid, Sid, Metadata], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Name, "roster", "add"),
    {ok, updated} = uce_meeting:join(Domain, Name, Uid),
    uce_presence:join(Domain, Sid, Name),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       type="internal.roster.add",
                                       location=Name,
                                       from=Uid,
                                       metadata=json_helpers:to_struct(Metadata)}),
    json_helpers:ok(Response).

%% TODO : Incomplete Sid must be ToSid
leave(Domain, [{meeting, Name}, {uid, User}], [Uid, Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Name, "roster", "delete"),
    {ok, updated} = uce_meeting:leave(Domain, Name, User),
    uce_presence:leave(Domain, Sid, Name),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       type="internal.roster.delete",
                                       location=Name,
                                       from=User}),
    json_helpers:ok(Response).

roster(Domain, [{meeting, Name}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Name, "roster", "list"),
    {ok, Roster} = uce_meeting:roster(Domain, Name),
    json_helpers:json(Response, Domain, lists:map(fun(Member) ->
                                                          {ok, User} = uce_user:get(Domain, Member),
                                                          User
                                                  end,
                                                  Roster)).
