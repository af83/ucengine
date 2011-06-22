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

setup() ->
    Hosts = config:get(hosts),
    {Domain, _Config} = hd(Hosts),
    Port = config:get(port),
    (list_to_atom(lists:concat([config:get(Domain, db), "_db"]))):drop(),
    setup_meetings(Domain),
    UsersUid = setup_users(Domain),
    [Domain, "http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ ?VERSION ++ "/", setup_testers(Domain, UsersUid)].

teardown([Domain, _, _Testers]) ->
    teardown_solr(Domain),
    ok.

setup_meetings(Domain) ->
    Now = utils:now(),
    case catch uce_meeting:add(Domain,
                              #uce_meeting{id="testmeeting",
                                           metadata=[{"description", "Meeting"}],
                                           start_date=Now,
                                           end_date=?NEVER_ENDING_MEETING}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason1} -> throw({error, Reason1})
    end,
    case catch uce_meeting:add(Domain,
                              #uce_meeting{id="closedmeeting",
                                           metadata=[{"description", "Meeting"}],
                                           start_date=Now,
                                           end_date=Now}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason2} -> throw({error, Reason2})
    end,
    case catch uce_meeting:add(Domain,
                          #uce_meeting{id="upcomingmeeting",
                                       metadata=[{"description", "Meeting"}],
                                       start_date=2569256203952,
                                       end_date=?NEVER_ENDING_MEETING}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason3} -> throw({error, Reason3})
    end,
    ok.

setup_users(Domain) ->
    case catch uce_role:add(Domain, #uce_role{id="default",
                                              acl=[#uce_access{action="add", object="presence"},
                                              #uce_access{action="delete", object="presence"}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason4} -> throw({error, Reason4})
    end,

    case catch uce_role:add(Domain, #uce_role{id="root",
                                   acl=[#uce_access{action="all", object="all"}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason5} -> throw({error, Reason5})
    end,

    case catch uce_role:add(Domain, #uce_role{id="participant",
                                   acl=[#uce_access{action="add", object="presence"},
                                        #uce_access{action="delete", object="presence"},
                                        #uce_access{action="add", object="event"},
                                        #uce_access{action="list", object="event"},
                                        #uce_access{action="get", object="infos"},
                                        #uce_access{action="add", object="roster"},
                                        #uce_access{action="get", object="meeting"},
                                        #uce_access{action="list", object="meeting"}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason6} -> throw({error, Reason6})
    end,

    case catch uce_role:add(Domain, #uce_role{id="testrole_location",
                                   acl=[#uce_access{action="testaction", object="testobject", conditions=[{"a", "b"}]}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason7} -> throw({error, Reason7})
    end,

    case catch uce_role:add(Domain, #uce_role{id="testrole_without_location",
                                   acl=[#uce_access{action="testaction_global", object="testobject_global", conditions=[{"c", "d"}]}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason8} -> throw({error, Reason8})
    end,

    case catch uce_role:add(Domain, #uce_role{id="anonymous",
                                   acl=[#uce_access{action="add", object="presence"},
                                   #uce_access{action="delete", object="presence"}]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason9} -> throw({error, Reason9})
    end,

    ParticipantUid = "participant.user@af83.com",

    case catch uce_user:add(Domain, #uce_user{id=ParticipantUid,
                                              name=ParticipantUid,
                                              auth="password",
                                              credential="pwd"}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason10} -> throw({error, Reason10})
    end,

    case catch uce_user:add_role(Domain, ParticipantUid, {"participant", ""}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason11} -> throw({error, Reason11})
    end,

    case catch uce_user:add_role(Domain, ParticipantUid, {"testrole_location", "testmeeting"}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason12} -> throw({error, Reason12})
    end,

    case catch uce_user:add_role(Domain, ParticipantUid, {"testrole_without_location", ""}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason13} -> throw({error, Reason13})
    end,

    case catch uce_user:add_role(Domain, ParticipantUid, {"participant", ""}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason14} -> throw({error, Reason14})
    end,


    AnonymousUid = "anonymous.user@af83.com",

    case catch uce_user:add(Domain, #uce_user{id=AnonymousUid,
                                   name=AnonymousUid,
                                   auth="none",
                                   roles=[]}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason15} -> throw({error, Reason15})
    end,

    case catch uce_user:add_role(Domain, AnonymousUid, {"anonymous", ""}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason16} -> throw({error, Reason16})
    end,

    case catch uce_user:add(Domain,
                 #uce_user{id="token.user@af83.com",
                           name="token.user@af83.com",
                           auth="token",
                           credential="4444"}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason17} -> throw({error, Reason17})
    end,

    case catch uce_user:add(Domain,
                 #uce_user{id="user_2",
                           name="user_2",
                           auth="password",
                           credential="pwd"}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason18} -> throw({error, Reason18})
    end,

    case catch uce_user:add(Domain,
                 #uce_user{id="user_3",
                           name="user_3",
                           auth="password",
                           credential="pwd"}) of
        {ok, _} -> ok;
        {error, conflict} -> ok;
        {error, Reason19} -> throw({error, Reason19})
    end,


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
