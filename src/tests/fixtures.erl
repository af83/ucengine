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
    [Domain, "http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/api/0.3/", setup_testers(Domain)].

teardown([Domain, _, _Testers]) ->
    apply(list_to_atom(atom_to_list(config:get(db)) ++ "_db"), drop, []),
    teardown_solr(Domain),
    ok.

setup_meetings(Domain) ->
    Now = utils:now(),
    catch uce_meeting:add(#uce_meeting{id={"testmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=Now,
                                       end_date=?NEVER_ENDING_MEETING}),
    catch uce_meeting:add(#uce_meeting{id={"closedmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=Now,
                                       end_date=Now}),
    catch uce_meeting:add(#uce_meeting{id={"upcomingmeeting", Domain},
                                       metadata=[{"description", "Meeting"}],
                                       start_date=2569256203952,
                                       end_date=?NEVER_ENDING_MEETING}),
    ok.

setup_users(Domain) ->
    ParticipantId = {"participant.user@af83.com", Domain},
    catch uce_user:add(#uce_user{id=ParticipantId,
                                 auth="password",
                                 credential="pwd"}),
    uce_acl:add(#uce_acl{user=ParticipantId,
                         action="add",
                         object="presence",
                         location={"", Domain}}),
    uce_acl:add(#uce_acl{user=ParticipantId,
                         action="delete",
                         object="presence",
                         location={"", Domain},
                         conditions=[{"user", "participant.user@af83.com"}]}),
    
    AnonymousId = {"anonymous.user@af83.com", Domain},
    catch uce_user:add(#uce_user{id=AnonymousId,
                                 auth="anonymous",
                                 credential=""}),
    uce_acl:add(#uce_acl{user=AnonymousId,
                         action="add",
                         object="presence",
                         location={"", Domain}}),
    uce_acl:add(#uce_acl{user=AnonymousId,
                         action="delete",
                         object="presence",
                         location={"", Domain},
                         conditions=[{"user", "anonymous.user@af83.com"}]}),

    catch uce_user:add(#uce_user{id={"token.user@af83.com", Domain},
                                 auth="token",
                                 credential="4444"}),
    
    catch uce_user:add(#uce_user{id={"user_2", Domain},
                                 auth="password",
                                 credential="pwd"}),
    catch uce_user:add(#uce_user{id={"user_3", Domain},
                                 auth="password",
                                 credential="pwd"}),

    ok.

setup_testers(Domain) ->
    RootUid = "root.user@af83.com",
    RootId = {RootUid, Domain},
    catch uce_user:add(#uce_user{id=RootId,
                                 auth="password",
                                 credential="pwd"}),
    {ok, created} = uce_acl:add(#uce_acl{user=RootId,
                                         action="all",
                                         object="all",
                                         location={"", Domain}}),
    {ok, RootSid} = uce_presence:add(#uce_presence{user=RootId,
                                                   domain=Domain,
                                                   auth="password",
                                                   metadata=[]}),
    UglyUid = "ugly.user@af83.com",
    UglyId = {UglyUid, Domain},
    catch uce_user:add(#uce_user{id=UglyId,
                                 auth="password",
                                 credential="pwd"}),
    {ok, UglySid} = uce_presence:add(#uce_presence{user=UglyId,
                                                   domain=Domain,
                                                   auth="password",
                                                   metadata=[]}),
    [{RootUid, RootSid}, {UglyUid, UglySid}].

teardown_solr(Domain) ->
    uce_event_solr_search:delete(Domain, "*:*"),
    ok.
