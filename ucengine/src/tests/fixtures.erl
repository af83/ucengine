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
-module(fixtures).

-include("uce.hrl").

-export([setup/0, teardown/1, get_default_domain/0, get_base_url/0]).

get_default_domain() ->
    Hosts = config:get(hosts),
    {Domain, _Config} = hd(Hosts),
    Domain.

get_base_url() ->
    Domain = get_default_domain(),
    Port = config:get(port),
    "http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ ?VERSION ++ "/".

setup() ->
    Domain = get_default_domain(),
    (list_to_atom(lists:concat([config:get(Domain, db), "_db"]))):drop(),
    drop_model(Domain, [uce_role, uce_user, uce_meeting, uce_file, uce_event, uce_infos]),
    setup_meetings(Domain),
    UsersUid = setup_users(Domain),
    [Domain, get_base_url(), setup_testers(Domain, UsersUid)].

drop_model(_Domain, []) ->
    ok;
drop_model(Domain, [Model|Models]) ->
    {exports, Funs} = proplists:lookup(exports, Model:module_info()),
    case proplists:lookup(drop, Funs) of
        {drop, 1} ->
            Model:drop(Domain);
        _ ->
            ok
    end,
    drop_model(Domain, Models).

teardown([Domain, _, _Testers]) ->
    teardown_solr(Domain),
    ok.

add_meeting(Domain, Meeting) ->
    case catch uce_meeting:add(Domain, Meeting) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

setup_meetings(Domain) ->
    Now = utils:now(),
    ok = add_meeting(Domain, #uce_meeting{id="testmeeting",
                                          metadata=[{"description", "Meeting"}],
                                          start_date=Now,
                                          end_date=?NEVER_ENDING_MEETING}),
    ok = add_meeting(Domain, #uce_meeting{id="closedmeeting",
                                          metadata=[{"description", "Meeting"}],
                                          start_date=Now,
                                          end_date=Now}),
    ok = add_meeting(Domain, #uce_meeting{id="upcomingmeeting",
                                          metadata=[{"description", "Meeting"}],
                                          start_date=2569256203952,
                                          end_date=?NEVER_ENDING_MEETING}),
    ok.


add_role(Domain, Role) ->
    case catch uce_role:add(Domain, Role) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

add_user(Domain, User) ->
    case catch uce_user:add(Domain, User) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

add_role_to_user(Domain, Uid, Params) ->
    case catch uce_user:add_role(Domain, Uid, Params) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason} -> throw({error, Reason})
    end.

setup_users(Domain) ->
    ok = add_role(Domain, #uce_role{id="default",
                                    acl=[#uce_access{action="add", object="presence"},
                                         #uce_access{action="delete", object="presence"}]}),
    ok = add_role(Domain, #uce_role{id="root",
                                    acl=[#uce_access{action="all", object="all"}]}),

    ok = add_role(Domain, #uce_role{id="participant",
                                    acl=[#uce_access{action="add", object="presence"},
                                         #uce_access{action="delete", object="presence"},
                                         #uce_access{action="add", object="event"},
                                         #uce_access{action="list", object="event"},
                                         #uce_access{action="get", object="infos"},
                                         #uce_access{action="add", object="roster"},
                                         #uce_access{action="list", object="roster"},
                                         #uce_access{action="get", object="meeting"},
                                         #uce_access{action="list", object="meeting"}]}),
    ok = add_role(Domain, #uce_role{id="testrole_location",
                                    acl=[#uce_access{action="testaction", object="testobject", conditions=[{"a", "b"}]}]}),

    ok = add_role(Domain, #uce_role{id="testrole_without_location",
                                    acl=[#uce_access{action="testaction_global", object="testobject_global", conditions=[{"c", "d"}]}]}),

    ok = add_role(Domain, #uce_role{id="anonymous",
                                    acl=[#uce_access{action="add", object="presence"},
                                         #uce_access{action="delete", object="presence"}]}),

    ParticipantUid = "participant.user@af83.com",
    ok = add_user(Domain, #uce_user{id=ParticipantUid,
                                    name=ParticipantUid,
                                    auth="password",
                                    credential="pwd"}),
    timer:sleep(10),
    ok = add_role_to_user(Domain, ParticipantUid, {"participant", ""}),
    ok = add_role_to_user(Domain, ParticipantUid, {"testrole_location", "testmeeting"}),
    ok = add_role_to_user(Domain, ParticipantUid, {"testrole_without_location", ""}),
    ok = add_role_to_user(Domain, ParticipantUid, {"participant", ""}),

    AnonymousUid = "anonymous.user@af83.com",
    ok = add_user(Domain, #uce_user{id=AnonymousUid,
                                    name=AnonymousUid,
                                    auth="none"}),
    timer:sleep(10),
    ok = add_role_to_user(Domain, AnonymousUid, {"anonymous", ""}),

    ok = add_user(Domain, #uce_user{id="token.user@af83.com",
                                    name="token.user@af83.com",
                                    auth="token",
                                    credential="4444"}),
    ok = add_user(Domain, #uce_user{id="user_2",
                                    name="user_2",
                                    auth="password",
                                    credential="pwd"}),
    ok = add_user(Domain, #uce_user{id="user_3",
                                    name="user_3",
                                    auth="password",
                                    credential="pwd"}),

    {ok, RootUid} = uce_user:add(Domain,
                                 #uce_user{name="root.user@af83.com",
                                           auth="password",
                                           credential="pwd"}),

    uce_role:add_access(Domain, RootUid, #uce_access{action="all", object="all"}),

    {ok, UglyUid} = uce_user:add(Domain,
                                 #uce_user{name="ugly.user@af83.com",
                                           auth="password",
                                           credential="pwd",
                                           roles=[]}),

    uce_user:delete_role(Domain, UglyUid, {"default", ""}),

    {RootUid, ParticipantUid, UglyUid, AnonymousUid}.

setup_testers(Domain, {RootUid, ParticipantUid, UglyUid, AnonymousUid}) ->
    {ok, RootSid} = uce_presence:add(Domain,
                                     #uce_presence{id=none,
                                                   user=RootUid,
                                                   auth="password",
                                                   metadata=[]}),

    {ok, ParticipantSid} = uce_presence:add(Domain,
                                            #uce_presence{id=none,
                                                          user=ParticipantUid,
                                                          auth="password",
                                                          metadata=[]}),

    {ok, UglySid} = uce_presence:add(Domain,
                                     #uce_presence{id=none,
                                                   user=UglyUid,
                                                   auth="password",
                                                   metadata=[]}),

    [{RootUid, RootSid}, {ParticipantUid, ParticipantSid}, {UglyUid, UglySid}, {AnonymousUid, ""}].

teardown_solr(Domain) ->
    uce_event_solr_search:delete(Domain, "*:*"),
    ok.
