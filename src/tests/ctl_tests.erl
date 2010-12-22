-module(ctl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

ctl_org_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(_Testers) ->
        [ ?_test(test_org_add())
        , ?_test(test_org_add_missing_parameter())
        , ?_test(test_org_get())
        , ?_test(test_org_get_missing_parameter())
        , ?_test(test_org_get_not_found())
        , ?_test(test_org_update())
        , ?_test(test_org_update_missing_parameter())
        , ?_test(test_org_update_not_found())
        , ?_test(test_org_delete())
        , ?_test(test_org_delete_missing_parameter())
        , ?_test(test_org_delete_not_found())
        , ?_test(test_org_list())
        ]
      end
    }.

ctl_meeting_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(_Testers) ->
        [ ?_test(test_meeting_add())
        , ?_test(test_meeting_add_missing_parameter())
        , ?_test(test_meeting_get())
        , ?_test(test_meeting_get_missing_parameter())
        , ?_test(test_meeting_get_not_found())
        , ?_test(test_meeting_update())
        , ?_test(test_meeting_update_missing_parameter())
        , ?_test(test_meeting_update_not_found())
        , ?_test(test_meeting_delete())
        , ?_test(test_meeting_delete_missing_parameter())
        , ?_test(test_meeting_delete_not_found())
        , ?_test(test_meeting_list())
        , ?_test(test_meeting_list_missing_parameter())
        , ?_test(test_meeting_list_not_found())
        ]
      end
    }.

ctl_user_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(_Testers) ->
        [ ?_test(test_user_add())
        , ?_test(test_user_add_missing_parameter())
        , ?_test(test_user_get())
        , ?_test(test_user_get_missing_parameter())
        , ?_test(test_user_get_not_found())
        , ?_test(test_user_update())
        , ?_test(test_user_update_missing_parameter())
        , ?_test(test_user_update_not_found())
        , ?_test(test_user_delete())
        , ?_test(test_user_delete_missing_parameter())
        , ?_test(test_user_delete_not_found())
        , ?_test(test_user_list())
        ]
      end
    }.


%%
%% Org
%%
test_org_add() ->
    {error, not_found} = uce_org:get("neworg"),
    Params = [{"name", ["neworg"]}, {"description", [""]}],
    ok = uce_ctl:action(org, add, Params),
    Expected = {ok, #uce_org{name="neworg", metadata=[{"description", ""}]}},
    Expected = uce_org:get("neworg").
test_org_add_missing_parameter() ->
    Params = [{"description", [""]}],
    error = uce_ctl:action(org, add, Params).

test_org_get() ->
    Params = [{"name", ["testorg"]}],
    ok = uce_ctl:action(org, get, Params).
test_org_get_missing_parameter() ->
    error = uce_ctl:action(org, get, []).
test_org_get_not_found() ->
    Params = [{"name", ["org that doesnt exists"]}],
    error = uce_ctl:action(org, get, Params).

test_org_update() ->
    Before = {ok, #uce_org{name="testorg", metadata=[{"description", "testorg"}]}},
    Before = uce_org:get("testorg"),
    Params = [{"name", ["testorg"]}, {"description", ["A new description"]}],
    ok = uce_ctl:action(org, update, Params),
    Expected = {ok, #uce_org{name="testorg", metadata=[{"description", "A new description"}]}},
    Expected = uce_org:get("testorg").
test_org_update_missing_parameter() ->
    error = uce_ctl:action(org, update, []).
test_org_update_not_found() ->
    Params = [{"name", ["org that doesnt exists"]}],
    error = uce_ctl:action(org, update, Params).

test_org_delete() ->
    Before = {ok, #uce_org{name="testorg", metadata=[{"description", "A new description"}]}},
    Before = uce_org:get("testorg"),
    Params = [{"name", ["testorg"]}],
    ok = uce_ctl:action(org, delete, Params),
    {error, not_found} = uce_org:get("testorg").
test_org_delete_missing_parameter() ->
    error = uce_ctl:action(org, delete, []).
test_org_delete_not_found() ->
    Params = [{"name", ["org that doesnt exists"]}],
    error = uce_ctl:action(org, delete, Params).

test_org_list() ->
    ok = uce_ctl:action(org, list, []).

%%
%% Meeting
%%

test_meeting_add() ->
    {error, not_found} = uce_meeting:get(["testorg", "newmeeting"]),
    Params = [{"org", ["testorg"]}, {"name", ["newmeeting"]}, {"description", [""]}],
    ok = uce_ctl:action(meeting, add, Params),
    Expected = {ok, #uce_meeting{id=["testorg", "newmeeting"],
                                 start_date=0, end_date=0,
                                 metadata=[{"description", ""}]}},
    Expected = uce_meeting:get(["testorg", "newmeeting"]).
test_meeting_add_missing_parameter() ->
    error = uce_ctl:action(meeting, add, []).

test_meeting_get() ->
    Params = [{"org", ["testorg"]}, {"name", ["testmeeting"]}],
    ok = uce_ctl:action(meeting, get, Params).
test_meeting_get_missing_parameter() ->
    error = uce_ctl:action(meeting, get, []).
test_meeting_get_not_found() ->
    Params = [{"org", ["testorg"]}, {"name", ["meeting that doesn't exists"]}],
    error = uce_ctl:action(meeting, get, Params).

test_meeting_update() ->
    {ok, #uce_meeting{ id=["testorg", "testmeeting"]
                     , start_date=Start
                     , end_date=End
                     , metadata=[{"description", Description}]
                     }} = uce_meeting:get(["testorg", "testmeeting"]),
    StartDate = uce_ctl:timestamp_to_iso(Start),
    EndDate = uce_ctl:timestamp_to_iso(End),
    Params = [ {"org", ["testorg"]}
             , {"name", ["testmeeting"]}
             , {"start", StartDate}
             , {"end", EndDate}
             , {"description", ["A new description"]}
             ],
    ok = uce_ctl:action(meeting, update, Params),
    Expected = {ok, #uce_meeting{ id=["testorg", "testmeeting"]
                                , start_date=uce_ctl:parse_date(StartDate)
                                , end_date=uce_ctl:parse_date(EndDate)
                                , metadata=[{"description", "A new description"}]
                                }},
    Expected = uce_meeting:get(["testorg", "testmeeting"]).
test_meeting_update_missing_parameter() ->
    error = uce_ctl:action(org, update, []).
test_meeting_update_not_found() ->
    Params = [{"org", ["testorg"]}, {"name", ["org that doesnt exists"]}],
    error = uce_ctl:action(org, update, Params).

test_meeting_delete() ->
    {ok, #uce_meeting{ id=["testorg", "testmeeting"]
                     , start_date=_Start
                     , end_date=_End
                     , metadata=[{"description", _Description}]
                     }} = uce_meeting:get(["testorg", "testmeeting"]),
    Params = [{"org", ["testorg"]}, {"name", ["testmeeting"]}],
    ok = uce_ctl:action(meeting, delete, Params),
    {error, not_found} = uce_meeting:get(["testorg", "testmeeting"]).
test_meeting_delete_missing_parameter() ->
    error = uce_ctl:action(meeting, delete, []).
test_meeting_delete_not_found() ->
    Params = [{"org", ["testorg"]}, {"name", ["meeting that doesn't exists"]}],
    error = uce_ctl:action(meeting, delete, []).

test_meeting_list() ->
    Params = [{"org", ["testorg"]}, {"status", ["all"]}],
    ok = uce_ctl:action(meeting, list, Params).
test_meeting_list_missing_parameter() ->
    error = uce_ctl:action(meeting, list, []).
test_meeting_list_not_found() ->
    Params = [{"org", ["org that doesn't exitst"]}, {"status", ["all"]}],
    error = uce_ctl:action(meeting, list, []).

%%
%% User
%%

test_user_add() ->
    {error, not_found} = uce_user:get("test.user@af83.com"),
    Params = [ {"uid", ["test.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(user, add, Params),
    {ok, #uce_user{uid="test.user@af83.com",
                   auth="password",
                   credential="pwd"}} = uce_user:get("test.user@af83.com").
test_user_add_missing_parameter() ->
    Params = [ {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    error = uce_ctl:action(user, add, Params).

test_user_get() ->
    Params = [ {"uid", ["participant.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(user, get, Params).
test_user_get_missing_parameter() ->
    Params = [{"auth", ["password"]}, {"credential", ["pwd"]}],
    error = uce_ctl:action(user, get, Params).
test_user_get_not_found() ->
    Params = [ {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    error = uce_ctl:action(user, get, Params).

test_user_update() ->
    {ok, #uce_user{uid="anonymous.user@af83.com",
                   auth="anonymous",
                   credential=""}} =
        uce_user:get("anonymous.user@af83.com"),
    Params = [ {"uid", ["anonymous.user@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["pwd"]}
             ],
    ok = uce_ctl:action(user, update, Params),
    {ok, #uce_user{uid="anonymous.user@af83.com",
                   auth="password",
                   credential="pwd"}} =
        uce_user:get("anonymous.user@af83.com").
test_user_update_missing_parameter() ->
    error = uce_ctl:action(org, update, []).
test_user_update_not_found() ->
    Params = [ {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["passwd"]}
             ],
    error = uce_ctl:action(user, update, Params).

test_user_delete() ->
    {ok, #uce_user{uid="participant.user@af83.com",
                   auth="password",
                   credential="pwd"}} = uce_user:get("participant.user@af83.com"),
    Params = [{"uid", ["participant.user@af83.com"]}],
    ok = uce_ctl:action(user, delete, Params),
    {error, not_found} = uce_user:get("participant.user@af83.com").
test_user_delete_missing_parameter() ->
    error = uce_ctl:action(user, delete, []).
test_user_delete_not_found() ->
    Params = [ {"uid", ["nobody@af83.com"]}
             , {"auth", ["password"]}
             , {"credential", ["passwd"]}
             ],
    error = uce_ctl:action(user, delete, Params).


test_user_list() ->
    ok = uce_ctl:action(user, list, []).

