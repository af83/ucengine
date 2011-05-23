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
-module(ctl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

ctl_meeting_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([Domain, _BaseUrl, _Testers]) ->
        [ ?_test(test_meeting_add(Domain))
        , ?_test(test_meeting_add_missing_parameter())
        % TODO: Test the conflict case
        , ?_test(test_meeting_get(Domain))
        , ?_test(test_meeting_get_missing_parameter())
        , ?_test(test_meeting_get_not_found(Domain))
        , ?_test(test_meeting_update(Domain))
        , ?_test(test_meeting_update_missing_parameter())
        , ?_test(test_meeting_update_not_found(Domain))
        , ?_test(test_meeting_delete(Domain))
        , ?_test(test_meeting_delete_missing_parameter())
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
        , ?_test(test_user_add_missing_parameter())
        % TODO: Test the conflict case
        , ?_test(test_user_get_by_name(Domain))
        , ?_test(test_user_get_missing_parameter(Domain))
        , ?_test(test_user_get_not_found(Domain))
        , ?_test(test_user_update(Domain))
        , ?_test(test_user_update_missing_parameter(Domain))
        , ?_test(test_user_update_not_found(Domain))
        , ?_test(test_user_add_role(Domain))
        , ?_test(test_user_delete_role(Domain))
        , ?_test(test_user_delete(Domain))
        , ?_test(test_user_delete_missing_parameter())
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
    false = uce_meeting:exists(Domain, {"newmeeting", Domain}),
    Params = [{"name", "newmeeting"}, {"start", 0}, {"end", 0}, {"description", ""}],
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "add"]}, Params),
    Expected = {ok, #uce_meeting{id={"newmeeting", Domain},
                                 start_date=0, end_date=0,
                                 metadata=[{"description", ""}]}},
    ?assertEqual(Expected, uce_meeting:get(Domain, {"newmeeting", Domain})).

test_meeting_add_missing_parameter() ->
    ?assertEqual(error, uce_ctl:cmd({dummy, ["", "meeting", "add"]}, [])).

test_meeting_get(Domain) ->
    Params = [{"name", "testmeeting"}],
    ?assertMatch({ok, _}, uce_ctl:cmd({dummy, [Domain, "meeting", "get"]}, Params)).

test_meeting_get_missing_parameter() ->
    ?assertMatch(error, uce_ctl:cmd({dummy, ["", "meeting", "get"]}, [])).

test_meeting_get_not_found(Domain) ->
    Params = [{"name", "meeting that doesn't exists"}],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "get"]}, Params)).

test_meeting_update(Domain) ->
    {ok, #uce_meeting{ id={"testmeeting", Domain}
                     , start_date=Start
                     , end_date=End
                     , metadata=[{"description", _Description}]
                     }} = uce_meeting:get(Domain, {"testmeeting", Domain}),
    StartDate = uce_ctl:timestamp_to_iso(Start),
    EndDate = uce_ctl:timestamp_to_iso(End),
    Params = [ {"name", "testmeeting"}
             , {"start", StartDate}
             , {"end", EndDate}
             , {"description", "A new description"}
             ],
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "update"]}, Params),
    Expected = {ok, #uce_meeting{ id={"testmeeting", Domain}
                                , start_date=uce_ctl:parse_date(StartDate)
                                , end_date=uce_ctl:parse_date(EndDate)
                                , metadata=[{"description", "A new description"}]
                                }},
    ?assertMatch(Expected, uce_meeting:get(Domain, {"testmeeting", Domain})).

test_meeting_update_missing_parameter() ->
    error = uce_ctl:cmd({dummy, ["", "meeting", "update"]}, []).

test_meeting_update_not_found(Domain) ->
    Params = [{"name", "meeting that doesnt exists"},
               {"start", uce_ctl:timestamp_to_iso()},
               {"end", uce_ctl:timestamp_to_iso()}],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "update"]}, Params)).

test_meeting_delete(Domain) ->
    {ok, #uce_meeting{ id={"testmeeting", Domain}
                     , start_date=_Start
                     , end_date=_End
                     , metadata=[{"description", _Description}]
                     }} = uce_meeting:get(Domain, {"testmeeting", Domain}),
    Params = [{"name", "testmeeting"}],
    ok = uce_ctl:cmd({dummy, [Domain, "meeting", "delete"]}, Params),
    false = uce_meeting:exists(Domain, {"testmeeting", Domain}).

test_meeting_delete_missing_parameter() ->
    error = uce_ctl:cmd({dummy, ["", "meeting", "delete"]}, []).

test_meeting_delete_not_found(Domain) ->
    Params = [{"name", "meeting that doesn't exists"}],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "meeting", "delete"]}, Params)).

test_meeting_list(Domain) ->
    Params = [{"status", "all"}],
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "meeting", "list"]}, Params).

%%
%% User
%%

test_user_add(Domain) ->
    false = uce_user:exists(Domain, {"test.user@af83.com", Domain}),
    Params = [{"name", "test.user@af83.com"}
             , {"auth", "password"}
             , {"credential", "pwd"}
             ],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "add"]}, Params),
    {ok, #uce_user{id={_, Domain},
                   name="test.user@af83.com",
                   auth="password",
                   credential="pwd",
                   metadata=[]}} = uce_user:get(Domain, "test.user@af83.com").

test_user_add_missing_parameter() ->
    Params = [ {"auth", "password"}
             , {"credential", "pwd"}
             ],
    error = uce_ctl:cmd({dummy, ["", "user", "add"]}, Params).

test_user_get_by_name(Domain) ->
    Params = [{"name", "participant.user@af83.com"}],
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "user", "get"]}, Params).

test_user_get_missing_parameter(Domain) ->
    error = uce_ctl:cmd({dummy, [Domain, "user", "get"]}, []).

test_user_get_not_found(Domain) ->
    Params = [{"name", "nobody@af83.com"}],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "get"]}, Params)).

test_user_update(Domain) ->
    {ok, #uce_user{id={Uid, Domain},
                   name="anonymous.user@af83.com",
                   auth="none"}} =
        uce_user:get(Domain, "anonymous.user@af83.com"),
    Params = [ {"uid", Uid}
             , {"name", "anonymous.user@af83.com"}
             , {"auth", "password"}
             , {"credential", "pwd"}
             ],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "update"]}, Params),
    {ok, #uce_user{id={Uid, Domain},
                   name="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd"}} =
        uce_user:get(Domain, "anonymous.user@af83.com").

test_user_update_missing_parameter(Domain) ->
    error = uce_ctl:cmd({dummy, [Domain, "user", "update"]}, [{"name", "anonymous.user@af83.com"}]).

test_user_update_not_found(Domain) ->
    Params = [ {"uid", "none"}
             , {"name", "nobody@af83.com"}
             , {"auth", "password"}
             , {"credential", "passwd"}
             ],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "update"]}, Params)).

test_user_add_role(Domain) ->
    {ok, #uce_user{id={Uid, Domain},
                   name="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd",
                   roles=[]}} =
        uce_user:get(Domain, "anonymous.user@af83.com"),
    Params = [ {"uid", Uid}
             , {"role", "root"}
             , {"location", "testmeeting"}
             ],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "role", "add"]}, Params),
    {ok, #uce_user{id={Uid, Domain},
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get(Domain, "anonymous.user@af83.com").

test_user_delete_role(Domain) ->
    {ok, #uce_user{id={Uid, Domain},
                   name="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get(Domain, "anonymous.user@af83.com"),
    Params = [{"uid", Uid}
             , {"role", "root"}
             , {"location", "testmeeting"}
             ],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "role", "delete"]}, Params),
    {ok, #uce_user{id={Uid, Domain},
                   auth="password",
                   credential="pwd",
                   roles=[]}} =
        uce_user:get(Domain, "anonymous.user@af83.com").

test_user_delete(Domain) ->
    {ok, #uce_user{id={Uid, Domain},
                   name="participant.user@af83.com",
                   auth="password",
                   credential="pwd"}} = uce_user:get(Domain, "participant.user@af83.com"),
    Params = [{"uid", Uid}],
    ok = uce_ctl:cmd({dummy, [Domain, "user", "delete"]}, Params),
    false = uce_user:exists(Domain, "participant.user@af83.com").

test_user_delete_missing_parameter() ->
    error = uce_ctl:cmd({dummy, ["", "user", "delete"]}, []).

test_user_delete_not_found(Domain) ->
    Params = [{"uid", "nobody@af83.com"}],
    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "user", "delete"]}, Params)).

test_user_list(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "user", "list"]}, []).

%%
%% Roles
%%
test_role_add(Domain) ->
    {error, not_found} = (catch uce_role:get(Domain, {"test_role", Domain})),
    Params = [{"name", "test_role"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "add"]}, Params),

    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}).

test_role_add_conflict(Domain) ->
    Params = [{"name", "test_role"}],

    {error, conflict} = (catch uce_ctl:cmd({dummy, [Domain, "role", "add"]}, Params)),

    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}).

test_role_delete(Domain) ->
    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}),
    Params = [{"name", "test_role"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "delete"]}, Params),
    {error, not_found} = (catch uce_role:get(Domain, {"test_role", Domain})).

test_role_delete_not_found(Domain) ->
    Params = [{"name", ["test_role"]}],

    {error, not_found} = (catch uce_ctl:cmd({dummy, [Domain, "role", "delete"]}, Params)).

test_role_add_access(Domain) ->
    uce_role:add(Domain, #uce_role{id={"test_role_2", Domain}}),
    Params = [{"name", "test_role_2"}
            , {"object", "testobject"}
            , {"action", "testaction"}
            , {"a", "b"}
            , {"c", "d"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "add"]}, Params),
    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "add"]}, Params),

    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
        uce_role:get(Domain, {"test_role_2", Domain}).

test_role_check_access(Domain) ->
    {ok, Anonymous} = uce_user:get(Domain, "anonymous.user@af83.com"),
    {AnonymousUid, _} = Anonymous#uce_user.id,
    uce_user:add_role(Domain, {AnonymousUid, Domain}, {"test_role_2", ""}),
    Params = [{"uid", AnonymousUid}
            , {"object", "testobject"}
            , {"action", "testaction"}
            , {"a", "b"}
            , {"c", "d"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "check"]}, Params).

test_role_delete_access(Domain) ->
    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
        uce_role:get(Domain, {"test_role_2", Domain}),
    Params = [{"name", "test_role_2"}
            , {"object", "testobject"}
            , {"action", "testaction"}
            , {"c", "d"}
            , {"a", "b"}],

    ok = uce_ctl:cmd({dummy, [Domain, "role", "access", "delete"]}, Params),
    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[]}} =
        uce_role:get(Domain, {"test_role_2", Domain}).

%%
%% Infos
%%

test_infos_get(Domain) ->
    {ok, _} = uce_ctl:cmd({dummy, [Domain, "infos", "get"]}, []).

test_infos_update(Domain) ->
    {ok, {uce_infos, Domain, []}} = uce_infos:get(Domain),
    Params = [{"description", "Informations"}],
    ok = uce_ctl:cmd({dummy, [Domain, "infos", "update"]}, Params),
    {ok, {uce_infos, Domain, [{"description", "Informations"}]}} = uce_infos:get(Domain).
