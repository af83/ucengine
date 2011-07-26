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
-module(ctl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

ctl_meeting_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([Domain, _BaseUrl, _Testers]) ->
        [ ?_test(test_meeting_add(Domain))
        % TODO: Test the conflict case
        , ?_test(test_meeting_get(Domain))
        , ?_test(test_meeting_get_not_found(Domain))
        , ?_test(test_meeting_update(Domain))
        , ?_test(test_meeting_update_not_found(Domain))
        , ?_test(test_meeting_delete(Domain))
        , ?_test(test_meeting_delete_not_found(Domain))
        , ?_test(test_meeting_list(Domain))
        ]
      end
    }.

ctl_user_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([Domain, _BaseUrl, _Testers]) ->
        [ ?_test(test_user_add(Domain))
        % TODO: Test the conflict case
        , ?_test(test_user_get(Domain))
        , ?_test(test_user_get_not_found(Domain))
        , ?_test(test_user_update(Domain))
        , ?_test(test_user_update_not_found(Domain))
        , ?_test(test_user_add_role(Domain))
        , ?_test(test_user_delete_role(Domain))
        , ?_test(test_user_delete(Domain))
        , ?_test(test_user_delete_not_found(Domain))
        , ?_test(test_user_list(Domain))
        ]
      end
    }.

ctl_roles_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([Domain, _BaseUrl, _Testers]) ->
                [ ?_test(test_role_add(Domain))
                ,  ?_test(test_role_add_conflict(Domain))
                ,  ?_test(test_role_delete(Domain))
                ,  ?_test(test_role_delete_not_found(Domain))
                ,  ?_test(test_role_add_access(Domain))
                ,  ?_test(test_role_check_access(Domain))
                ,  ?_test(test_role_delete_access(Domain))
                ]
        end
    }.

ctl_infos_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([Domain, _BaseUrl, _Testers]) ->
                [ ?_test(test_infos_get(Domain))
                ,  ?_test(test_infos_update(Domain))
                ]
        end
    }.

%%
%% Meeting
%%

test_meeting_add(Domain) ->
    false = uce_meeting:exists(Domain, "newmeeting"),
    Params = [{"description", ""}],
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "add", "newmeeting"]}, Params),
    Expected = {ok, #uce_meeting{id="newmeeting",
                                 start_date=0, end_date=0,
                                 metadata=json_helpers:to_struct([{"description", ""}])}},
    ?assertEqual(Expected, uce_meeting:get(Domain, "newmeeting")).

test_meeting_get(Domain) ->
    ?assertMatch({ok, _}, uce_ctl:cmd({dummy, [Domain, "meeting", "get", "testmeeting"]}, [])).

test_meeting_get_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "get", "meeting that doesn't exists"]}, [])).

test_meeting_update(Domain) ->
    {ok, #uce_meeting{ id="testmeeting"
                     , start_date=Start
                     , end_date=End
                     , metadata={struct, [{"description", _Description}]}
                     }} = uce_meeting:get(Domain, "testmeeting"),
    StartDate = uce_ctl:timestamp_to_iso(Start),
    EndDate = uce_ctl:timestamp_to_iso(End),
    Params = [{"description", "A new description"}, {"start", StartDate}, {"end", EndDate}],
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "update", "testmeeting"]}, Params),
    Expected = {ok, #uce_meeting{ id="testmeeting"
                                , start_date=uce_ctl:parse_date(StartDate)
                                , end_date=uce_ctl:parse_date(EndDate)
                                , metadata=json_helpers:to_struct([{"description", "A new description"}])
                                }},
    ?assertMatch(Expected, uce_meeting:get(Domain, "testmeeting")).

test_meeting_update_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "update",
                                                     "meeting that doesnt exists"]}, [])).

test_meeting_delete(Domain) ->
    {ok, #uce_meeting{ id="testmeeting"
                     , start_date=_Start
                     , end_date=_End
                     , metadata={struct, [{"description", _Description}]}
                     }} = uce_meeting:get(Domain, "testmeeting"),
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "delete", "testmeeting"]}, []),
    false = uce_meeting:exists(Domain, "testmeeting").

test_meeting_delete_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "delete", "meeting that doesn't exists"]}, [])).

test_meeting_list(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "meeting", "list", "all"]}, []).

%%
%% User
%%

test_user_add(Domain) ->
    ?assertMatch(false, uce_user:exists(Domain, "test.user@af83.com")),
    ?assertMatch(ok, uce_ctl:cmd({dummy, [Domain, "user", "add", "test.user@af83.com", "password", "pwd"]}, [])),
    ?assertMatch({ok, #uce_user{id=_,
                                name="test.user@af83.com",
                                auth="password",
                                credential="pwd",
                                metadata={struct, []}}}, uce_user:get_by_name(Domain, "test.user@af83.com")).

test_user_get(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "user", "get", "participant.user@af83.com"]}, []).

test_user_get_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "get", "nobody@af83.com"]}, [])).

test_user_update(Domain) ->
    {ok, #uce_user{id=Uid,
                   name="anonymous.user@af83.com",
                   auth="none"}} =
        uce_user:get_by_name(Domain, "anonymous.user@af83.com"),
    ok = uce_ctl:cmd({dummy, [Domain, "user", "update", "anonymous.user@af83.com", "password", "pwd"]}, []),
    {ok, #uce_user{id=Uid,
                   name="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd"}} =
        uce_user:get_by_name(Domain, "anonymous.user@af83.com").

test_user_update_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "update", "nobody@af83.com", "password", "passwd"]}, [])).

test_user_add_role(Domain) ->
    Params = [{"location", "testmeeting"}],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "role", "add", "anonymous.user@af83.com", "root"]}, Params),
    {ok, #uce_user{id=_Uid,
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get_by_name(Domain, "anonymous.user@af83.com").

test_user_delete_role(Domain) ->
    {ok, #uce_user{id=_Uid,
                   name="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get_by_name(Domain, "anonymous.user@af83.com"),
    Params = [{"location", "testmeeting"}],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "role", "delete", "anonymous.user@af83.com", "root"]}, Params),
    {ok, #uce_user{id=_Uid,
                   auth="password",
                   credential="pwd",
                   roles=[]}} =
        uce_user:get_by_name(Domain, "anonymous.user@af83.com").

test_user_delete(Domain) ->
    true = uce_user:exists(Domain, "participant.user@af83.com"),
    ok = uce_ctl:cmd({dummy, [Domain, "user", "delete", "participant.user@af83.com"]}, []),
    false = uce_user:exists(Domain, "participant.user@af83.com").

test_user_delete_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "delete", "nobody@af83.com"]}, [])).

test_user_list(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "user", "list"]}, []).

%%
%% Roles
%%
test_role_add(Domain) ->
    {error, not_found} = (catch uce_role:get(Domain, "test_role")),
    ok = uce_ctl:cmd({dummy, [Domain, "role", "add", "test_role"]}, []),
    {ok, #uce_role{id="test_role", acl=[]}} = uce_role:get(Domain, "test_role").

test_role_add_conflict(Domain) ->
    {error, conflict} = (catch uce_ctl:cmd({dummy, [Domain, "role", "add", "test_role"]}, [])),
    {ok, #uce_role{id="test_role", acl=[]}} = uce_role:get(Domain, "test_role").

test_role_delete(Domain) ->
    {ok, #uce_role{id="test_role", acl=[]}} = uce_role:get(Domain, "test_role"),
    ok = uce_ctl:cmd({dummy, [Domain, "role", "delete", "test_role"]}, []),
    {error, not_found} = (catch uce_role:get(Domain, "test_role")).

test_role_delete_not_found(Domain) ->
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "role", "delete", "test_role"]}, [])).

test_role_add_access(Domain) ->
    uce_role:add(Domain, #uce_role{id="test_role_2"}),
    Params = [ {"a", "b"}
             , {"c", "d"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "add", "test_role_2", "testaction", "testobject"]}, Params),

    {ok, #uce_role{id="test_role_2",
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
                 uce_role:get(Domain, "test_role_2").

test_role_check_access(Domain) ->
    {ok, Anonymous} = uce_user:get_by_name(Domain, "anonymous.user@af83.com"),
    AnonymousUid = Anonymous#uce_user.id,

    uce_user:add_role(Domain, AnonymousUid, {"test_role_2", ""}),
    Params = [ {"a", "b"}
             , {"c", "d"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "check", "anonymous.user@af83.com", "testobject", "testaction"]}, Params).

test_role_delete_access(Domain) ->
    {ok, #uce_role{id="test_role_2",
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
        uce_role:get(Domain, "test_role_2"),
    Params = [ {"c", "d"}
             , {"a", "b"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "delete", "test_role_2", "testaction", "testobject"]}, Params),
    ?assertMatch({ok, #uce_role{id="test_role_2",
                   acl=[]}}, uce_role:get(Domain, "test_role_2")).

%%
%% Infos
%%

test_infos_get(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "infos", "get"]}, []).

test_infos_update(Domain) ->
    {ok, {uce_infos, Domain, {struct, []}}} = uce_infos:get(Domain),
    Params = {struct, [{"description", "Informations"}]},
    ok = uce_ctl:cmd({dummy, [Domain, "infos", "update"]}, Params),
    {ok, {uce_infos, Domain, {struct, [{"description", "Informations"}]}}} = uce_infos:get(Domain).
