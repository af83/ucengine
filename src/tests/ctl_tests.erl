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
        , ?_test(test_meeting_list_missing_parameter())
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
        , ?_test(test_user_get(Domain))
        , ?_test(test_user_get_missing_parameter())
        , ?_test(test_user_get_not_found(Domain))
        , ?_test(test_user_update(Domain))
        , ?_test(test_user_update_missing_parameter())
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
    Params = [{"domain", [Domain]}, {"name", ["newmeeting"]}, {"description", [""]}],
    ok = uce_ctl:action(["meeting", "add"], Params),
    Expected = {ok, #uce_meeting{id={"newmeeting", Domain},
                                 start_date=0, end_date=0,
                                 metadata=[{"description", ""}]}},
    Expected = uce_meeting:get(Domain, {"newmeeting", Domain}).
test_meeting_add_missing_parameter() ->
    error = uce_ctl:action(["meeting", "add"], []).

test_meeting_get(Domain) ->
    Params = [{"domain", [Domain]}, {"name", ["testmeeting"]}],
    ok = uce_ctl:action(["meeting", "get"], Params).
test_meeting_get_missing_parameter() ->
    error = uce_ctl:action(["meeting", "get"], []).
test_meeting_get_not_found(Domain) ->
    Params = [{"domain", [Domain]}, {"name", ["meeting that doesn't exists"]}],
    {error, not_found} = (catch uce_ctl:action(["meeting", "get"], Params)).

test_meeting_update(Domain) ->
    {ok, #uce_meeting{ id={"testmeeting", Domain}
                     , start_date=Start
                     , end_date=End
                     , metadata=[{"description", _Description}]
                     }} = uce_meeting:get(Domain, {"testmeeting", Domain}),
    StartDate = uce_ctl:timestamp_to_iso(Start),
    EndDate = uce_ctl:timestamp_to_iso(End),
    Params = [{"domain", [Domain]}
             , {"name", ["testmeeting"]}
             , {"start", [StartDate]}
             , {"end", [EndDate]}
             , {"description", ["A new description"]}
             ],
    ok = uce_ctl:action(["meeting", "update"], Params),
    Expected = {ok, #uce_meeting{ id={"testmeeting", Domain}
                                , start_date=uce_ctl:parse_date(StartDate)
                                , end_date=uce_ctl:parse_date(EndDate)
                                , metadata=[{"description", "A new description"}]
                                }},
    Expected = uce_meeting:get(Domain, {"testmeeting", Domain}).
test_meeting_update_missing_parameter() ->
    error = uce_ctl:action(["meeting", "update"], []).
test_meeting_update_not_found(Domain) ->
    Params = [{"domain", [Domain]}, {"name", ["meeting that doesnt exists"]}],
    {error, not_found} = (catch uce_ctl:action(["meeting", "update"], Params)).

test_meeting_delete(Domain) ->
    {ok, #uce_meeting{ id={"testmeeting", Domain}
                     , start_date=_Start
                     , end_date=_End
                     , metadata=[{"description", _Description}]
                     }} = uce_meeting:get(Domain, {"testmeeting", Domain}),
    Params = [{"domain", [Domain]}, {"name", ["testmeeting"]}],
    ok = uce_ctl:action(["meeting", "delete"], Params),
    false = uce_meeting:exists(Domain, {"testmeeting", Domain}).
test_meeting_delete_missing_parameter() ->
    error = uce_ctl:action(["meeting", "delete"], []).
test_meeting_delete_not_found(Domain) ->
    Params = [{"domain", [Domain]}, {"name", ["meeting that doesn't exists"]}],
    {error, not_found} = (catch uce_ctl:action(["meeting", "delete"], Params)).

test_meeting_list(Domain) ->
    Params = [{"domain", [Domain]}, {"status", ["all"]}],
    ok = uce_ctl:action(["meeting", "list"], Params).
test_meeting_list_missing_parameter() ->
    error = uce_ctl:action(["meeting", "list"], []).

%%
%% User
%%

test_user_add(Domain) ->
    false = uce_user:exists(Domain, {"test.user@af83.com", Domain}),
    Params = [ {"domain", [Domain]}
             , {"uid", ["test.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(["user", "add"], Params),
    {ok, #uce_user{id={"test.user@af83.com", Domain},
                   auth="password",
                   credential="pwd",
                   metadata=[]}} = uce_user:get(Domain, {"test.user@af83.com", Domain}).
test_user_add_missing_parameter() ->
    Params = [ {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    error = uce_ctl:action(["user", "add"], Params).

test_user_get(Domain) ->
    Params = [ {"domain", [Domain]}
             , {"uid", ["participant.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(["user", "get"], Params).

test_user_get_missing_parameter() ->
    Params = [{"auth", ["password"]}, {"credential", ["pwd"]}],
    error = uce_ctl:action(["user", "get"], Params).
test_user_get_not_found(Domain) ->
    Params = [ {"domain", [Domain]}
             , {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    {error, not_found} = (catch uce_ctl:action(["user", "get"], Params)).

test_user_update(Domain) ->
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="none"}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}),
    Params = [ {"domain", [Domain]}
             , {"uid", ["anonymous.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(["user", "update"], Params),
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="password",
                   credential="pwd"}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}).
test_user_update_missing_parameter() ->
    error = uce_ctl:action(["user", "update"], []).
test_user_update_not_found(Domain) ->
    Params = [ {"domain", [Domain]}
             , {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["passwd"]}
             ],
    {error, not_found} = (catch uce_ctl:action(["user", "update"], Params)).

test_user_add_role(Domain) ->
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="password",
                   credential="pwd",
                   roles=[]}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}),
    Params = [ {"domain", [Domain]}
             , {"uid", ["anonymous.user@af83.com"]}
             , {"role", ["root"]}
             , {"location", ["testmeeting"]}
             ],
    ok = uce_ctl:action(["user", "role", "add"], Params),
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}).

test_user_delete_role(Domain) ->
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="password",
                   credential="pwd",
                   roles=[{"root","testmeeting"}]}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}),
    Params = [ {"domain", [Domain]}
             , {"uid", ["anonymous.user@af83.com"]}
             , {"role", ["root"]}
             , {"location", ["testmeeting"]}
             ],
    ok = uce_ctl:action(["user", "role", "delete"], Params),
    {ok, #uce_user{id={"anonymous.user@af83.com", Domain},
                   auth="password",
                   credential="pwd",
                   roles=[]}} =
        uce_user:get(Domain, {"anonymous.user@af83.com", Domain}).

test_user_delete(Domain) ->
    {ok, #uce_user{id={"participant.user@af83.com", Domain},
                   auth="password",
                   credential="pwd"}} = uce_user:get(Domain, {"participant.user@af83.com", Domain}),
    Params = [{"domain", [Domain]}, {"uid", ["participant.user@af83.com"]}],
    ok = uce_ctl:action(["user", "delete"], Params),
    false = uce_user:exists(Domain, {"participant.user@af83.com", Domain}).
test_user_delete_missing_parameter() ->
    error = uce_ctl:action(["user", "delete"], []).
test_user_delete_not_found(Domain) ->
    Params = [ {"domain", [Domain]}
             , {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["passwd"]}
             ],
    {error, not_found} = (catch uce_ctl:action(["user", "delete"], Params)).

test_user_list(Domain) ->
    ok = uce_ctl:action(["user", "list"], [{"domain", [Domain]}]).

%%
%% Roles
%%
test_role_add(Domain) ->
    {error, not_found} = (catch uce_role:get(Domain, {"test_role", Domain})),
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role"]}],
    
    ok = uce_ctl:action(["role", "add"], Params),

    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}).

test_role_add_conflict(Domain) ->
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role"]}],
    
    {error, conflict} = (catch uce_ctl:action(["role", "add"], Params)),

    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}).

test_role_delete(Domain) ->
    {ok, #uce_role{id={"test_role", Domain}, acl=[]}} = uce_role:get(Domain, {"test_role", Domain}),
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role"]}],
    
    ok = uce_ctl:action(["role", "delete"], Params),
    {error, not_found} = (catch uce_role:get(Domain, {"test_role", Domain})).

test_role_delete_not_found(Domain) ->
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role"]}],
    
    {error, not_found} = (catch uce_ctl:action(["role", "delete"], Params)).

test_role_add_access(Domain) ->
    uce_role:add(Domain, #uce_role{id={"test_role_2", Domain}}),
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role_2"]}
               , {"object", ["testobject"]}
               , {"action", ["testaction"]}
               , {"a", ["b"]}
               , {"c", ["d"]}],
    
    ok = uce_ctl:action(["role", "access", "add"], Params),
    ok = uce_ctl:action(["role", "access", "add"], Params),

    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
        uce_role:get(Domain, {"test_role_2", Domain}).

test_role_delete_access(Domain) ->
    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[#uce_access{object="testobject",
                                    action="testaction",
                                    conditions=[{"a", "b"}, {"c", "d"}]}]}} =
        uce_role:get(Domain, {"test_role_2", Domain}),
    Params = [ {"domain", [Domain]}
               , {"name", ["test_role_2"]}
               , {"object", ["testobject"]}
               , {"action", ["testaction"]}
               , {"c", ["d"]}
               , {"a", ["b"]}],
    
    ok = uce_ctl:action(["role", "access", "delete"], Params),
    {ok, #uce_role{id={"test_role_2", Domain},
                   acl=[]}} =
        uce_role:get(Domain, {"test_role_2", Domain}).

%%
%% Infos
%%

test_infos_get(Domain) ->
    ok = uce_ctl:action(["infos", "get"], [{"domain", [Domain]}]).

test_infos_update(Domain) ->
    {ok, {uce_infos, Domain, []}} = uce_infos:get(Domain),
    Params = [{"domain", [Domain]}, {"description", ["Informations"]}],
    ok = uce_ctl:action(["infos", "update"], Params),
    {ok, {uce_infos, Domain, [{"description", "Informations"}]}} = uce_infos:get(Domain).
