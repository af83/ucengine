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

-export([setup/0, teardown/1]).

-compile([export_all]).

setup() ->
    Hosts = config:get(hosts),
    {Domain, _Config} = hd(Hosts),
    Port = config:get(port),
    setup_meetings(Domain),
    UsersUid = setup_users(Domain),
    [Domain, "http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ ?VERSION ++ "/", setup_testers(Domain, UsersUid)].

teardown([Domain, _, _Testers]) ->
    apply(list_to_atom(lists:concat([config:get(Domain, db), "_db"])), drop, []),
    teardown_solr(Domain),
    ok.

setup_meetings(Domain) ->
    Now = utils:now(),
    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"testmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=Now,
                                       end_date=?NEVER_ENDING_MEETING}),
    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"closedmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=Now,
                                       end_date=Now}),
    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"upcomingmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=2569256203952,
                                       end_date=?NEVER_ENDING_MEETING}),
    ok.

setup_users(Domain) ->
    catch uce_role:add(Domain, #uce_role{id={"default", Domain},
                                         acl=[#uce_access{action="add", object="presence"},
                                              #uce_access{action="delete", object="presence"}]}),

    catch uce_role:add(Domain, #uce_role{id={"root", Domain},
                                         acl=[#uce_access{action="all", object="all"}]}),

    catch uce_role:add(Domain, #uce_role{id={"participant", Domain},
                                         acl=[#uce_access{action="add", object="presence"},
                                              #uce_access{action="delete", object="presence"},
                                              #uce_access{action="add", object="event"},
                                              #uce_access{action="list", object="event"},
                                              #uce_access{action="get", object="infos"},
                                              #uce_access{action="add", object="roster"},
                                              #uce_access{action="get", object="meeting"},
                                              #uce_access{action="list", object="meeting"}]}),
    catch uce_role:add(Domain, #uce_role{id={"testrole_location", Domain},
                                         acl=[#uce_access{action="testaction", object="testobject", conditions=[{"a", "b"}]}]}),
    catch uce_role:add(Domain, #uce_role{id={"testrole_without_location", Domain},
                                         acl=[#uce_access{action="testaction_global", object="testobject_global", conditions=[{"c", "d"}]}]}),

    catch uce_role:add(Domain, #uce_role{id={"anonymous", Domain},
                                         acl=[#uce_access{action="add", object="presence"},
                                              #uce_access{action="delete", object="presence"}]}),

    ParticipantUid = "participant.user@af83.com",
    ParticipantId = {ParticipantUid, Domain},
    catch uce_user:add(Domain, #uce_user{id=ParticipantId,
                                         name=ParticipantUid,
                                         auth="password",
                                         credential="pwd"}),

    uce_user:addRole(Domain, ParticipantId, {"participant", ""}),
    uce_user:addRole(Domain, ParticipantId, {"testrole_location", "testmeeting"}),
    uce_user:addRole(Domain, ParticipantId, {"testrole_without_location", ""}),
    uce_user:addRole(Domain, ParticipantId, {"participant", ""}),

    AnonymousUid = "anonymous.user@af83.com",
    AnonymousId = {AnonymousUid, Domain},
    catch uce_user:add(Domain, #uce_user{id=AnonymousId,
                                         name=AnonymousUid,
                                         auth="none",
                                         roles=[]}),
    uce_user:addRole(Domain, AnonymousId, {"anonymous", ""}),

    catch uce_user:add(Domain,
                       #uce_user{id={"token.user@af83.com", Domain},
                                 name="token.user@af83.com",
                                 auth="token",
                                 credential="4444"}),

    catch uce_user:add(Domain,
                       #uce_user{id={"user_2", Domain},
                                 name="user_2",
                                 auth="password",
                                 credential="pwd"}),

    catch uce_user:add(Domain,
                       #uce_user{id={"user_3", Domain},
                                 name="user_3",
                                 auth="password",
                                 credential="pwd"}),

    {ok, RootUid} = uce_user:add(Domain,
                                 #uce_user{name="root.user@af83.com",
                                           auth="password",
                                           credential="pwd"}),
    catch uce_role:addAccess(Domain, {RootUid, Domain}, #uce_access{action="all", object="all"}),

    {ok, UglyUid} = uce_user:add(Domain,
                                 #uce_user{name="ugly.user@af83.com",
                                           auth="password",
                                           credential="pwd",
                                           roles=[]}),
    uce_user:deleteRole(Domain, {UglyUid, Domain}, {"default", ""}),

    {RootUid, ParticipantUid, UglyUid, AnonymousUid}.

setup_testers(Domain, {RootUid, ParticipantUid, UglyUid, AnonymousUid}) ->
    {ok, {RootSid, _Domain}} = uce_presence:add(Domain,
                                                 #uce_presence{id={none, Domain},
                                                               user={RootUid, Domain},
                                                               auth="password",
                                                               metadata=[]}),

    {ok, {ParticipantSid, _Domain}} = uce_presence:add(Domain,
                                                       #uce_presence{id={none, Domain},
                                                                     user={ParticipantUid, Domain},
                                                                     auth="password",
                                                                     metadata=[]}),

    {ok, {UglySid, _Domain}} = uce_presence:add(Domain,
                                                 #uce_presence{id={none, Domain},
                                                               user={UglyUid, Domain},
                                                               auth="password",
                                                               metadata=[]}),

    [{RootUid, RootSid}, {ParticipantUid, ParticipantSid}, {UglyUid, UglySid}, {AnonymousUid, ""}].

teardown_solr(Domain) ->
    uce_event_solr_search:delete(Domain, "*:*"),
    ok.
