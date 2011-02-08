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
    Domain = config:get(default_domain),
    Port = config:get(port),
    setup_meetings(Domain),
    setup_users(Domain),
    [Domain, "http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/api/0.2/", setup_testers(Domain)].

teardown([Domain, _, _Testers]) ->
    apply(list_to_atom(atom_to_list(config:get(db)) ++ "_db"), drop, []),
    teardown_solr(Domain),
    ok.

setup_meetings(Domain) ->
    Now = utils:now(),
    uce_meeting:add(Domain, #uce_meeting{id=["testmeeting"],
                                         metadata=[{"description", "Meeting"}],
                                         start_date=Now,
                                         end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(Domain, #uce_meeting{id=["closedmeeting"],
                                         metadata=[{"description", "Meeting"}],
                                         start_date=Now,
                                         end_date=Now}),
    uce_meeting:add(Domain, #uce_meeting{id=["upcomingmeeting"],
                                         metadata=[{"description", "Meeting"}],
                                         start_date=2569256203952,
                                         end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(Domain, #uce_meeting{id=["testmeeting"],
                                         metadata=[{"description", "Meeting"}],
                                         start_date=Now,
                                         end_date=?NEVER_ENDING_MEETING}),
    ok.

setup_users(Domain) ->
    uce_user:add(Domain, #uce_user{uid="participant.user@af83.com",
                                   auth="password",
                                   credential="pwd"}),
    uce_acl:add(Domain, #uce_acl{uid="participant.user@af83.com",
                                 action="add",
                                 object="presence"}),
    uce_acl:add(Domain, #uce_acl{uid="participant.user@af83.com",
                                 action="delete",
                                 object="presence",
                                 conditions=[{"user", "participant.user@af83.com"}]}),

    uce_user:add(Domain, #uce_user{uid="anonymous.user@af83.com", auth="anonymous", credential=""}),
    uce_acl:add(Domain, #uce_acl{uid="anonymous.user@af83.com",
                                 action="add",
                                 object="presence"}),
    uce_acl:add(Domain, #uce_acl{uid="anonymous.user@af83.com",
                                 action="delete",
                                 object="presence",
                                 conditions=[{"user", "anonymous.user@af83.com"}]}),

    uce_user:add(Domain, #uce_user{uid="token.user@af83.com", auth="token", credential="4444"}),
    ok.

setup_testers(Domain) ->
    RootUid = "root.user@af83.com",
    {ok, created} = uce_user:add(Domain, #uce_user{uid=RootUid,
                                                   auth="password",
                                                   credential="pwd"}),
    {ok, created} = uce_acl:add(Domain, #uce_acl{uid=RootUid,
                                                 action="all",
                                                 object="all"}),
    {ok, RootSid} = uce_presence:add(Domain, #uce_presence{uid=RootUid,
                                                           auth="password",
                                                           metadata=[]}),
    UglyUid = "ugly.user@af83.com",
    {ok, created} = uce_user:add(Domain, #uce_user{uid=UglyUid,
                                                   auth="password",
                                                   credential="pwd"}),
    {ok, UglySid} = uce_presence:add(Domain, #uce_presence{uid=UglyUid,
                                                           auth="password",
                                                           metadata=[]}),
    [{RootUid, RootSid}, {UglyUid, UglySid}].

teardown_solr(Domain) ->
    uce_event_solr_search:delete(Domain, "*:*"),
    ok.
