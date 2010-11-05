-module(meeting_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

meeting_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun({ROOT_UID, ROOT_SID}) ->
        [ ?_test(test_create(ROOT_UID, ROOT_SID))
	  , ?_test(test_create_conflict(ROOT_UID, ROOT_SID))
	  
	  , ?_test(test_get(ROOT_UID, ROOT_SID))
	  , ?_test(test_get_not_found_meeting(ROOT_UID, ROOT_SID))
	  , ?_test(test_get_not_found_org(ROOT_UID, ROOT_SID))
	  
	  , ?_test(test_list_all(ROOT_UID, ROOT_SID))
	  , ?_test(test_list_upcoming(ROOT_UID, ROOT_SID))
	  , ?_test(test_list_closed(ROOT_UID, ROOT_SID))
	  , ?_test(test_list_open(ROOT_UID, ROOT_SID))
	  , ?_test(test_list_bad_parameters(ROOT_UID, ROOT_SID))
	  , ?_test(test_list_not_found(ROOT_UID, ROOT_SID))

	  , ?_test(test_setup(ROOT_UID, ROOT_SID))
	  , ?_test(test_setup_not_found_meeting(ROOT_UID, ROOT_SID))
	  , ?_test(test_setup_not_found_org(ROOT_UID, ROOT_SID))

	  , ?_test(test_join(ROOT_UID, ROOT_SID))
	  , ?_test(test_join_not_found_meeting(ROOT_UID, ROOT_SID))
	  , ?_test(test_join_not_found_org(ROOT_UID, ROOT_SID))
	  , ?_test(test_join_not_founduid(ROOT_UID, ROOT_SID))

	  , ?_test(test_leave(ROOT_UID, ROOT_SID))
	  , ?_test(test_leave_not_found_meeting(ROOT_UID, ROOT_SID))
	  , ?_test(test_leave_not_found_org(ROOT_UID, ROOT_SID))
	  , ?_test(test_leave_not_founduid(ROOT_UID, ROOT_SID))
        ]
      end
    }.

test_create(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}
	     , {"start", integer_to_list(utils:now())}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"result", "created"}]} = tests_utils:put("/meeting/testorg/all/newmeeting", Params).

test_create_conflict(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}
	     , {"start", integer_to_list(utils:now())}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "conflict"}]} = tests_utils:put("/meeting/testorg/all/newmeeting", Params).

test_get(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}],
    {struct, [{"result",
	       {struct,
		[{"org", "testorg"},
		 {"name", "newmeeting"},
		 {"start_date",_},
		 {"end_date","never"},
		 {"roster",{array, []}},
		 {"metadata",{struct, [{"description", "Meeting"}]}}]}}]} = tests_utils:get("/meeting/testorg/all/newmeeting", Params).

test_get_not_found_meeting(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:get("/meeting/testorg/all/unexistentmeeting", Params).

test_get_not_found_org(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:get("/meeting/unexistentorg/all/newmeeting", Params).

test_list_all(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    JSON = tests_utils:get("/meeting/testorg/all", Params),
    test_meeting_in_list(["testorg", "testmeeting"], JSON),
    test_meeting_in_list(["testorg", "closedmeeting"], JSON),
    test_meeting_in_list(["testorg", "upcomingmeeting"], JSON).

test_list_upcoming(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    JSON = tests_utils:get("/meeting/testorg/upcoming", Params),
    test_meeting_not_in_list(["testorg", "testmeeting"], JSON),
    test_meeting_not_in_list(["testorg", "closedmeeting"], JSON),
    test_meeting_in_list(["testorg", "upcomingmeeting"], JSON).

test_list_closed(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    JSON = tests_utils:get("/meeting/testorg/closed", Params),
    test_meeting_not_in_list(["testorg", "testmeeting"], JSON),
    test_meeting_in_list(["testorg", "closedmeeting"], JSON),
    test_meeting_not_in_list(["testorg", "upcomingmeeting"], JSON).

test_list_open(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    JSON = tests_utils:get("/meeting/testorg/opened", Params),
    test_meeting_in_list(["testorg", "testmeeting"], JSON),
    test_meeting_not_in_list(["testorg", "closedmeeting"], JSON),
    test_meeting_not_in_list(["testorg", "upcomingmeeting"], JSON).

test_list_bad_parameters(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "bad_parameters"}]} = tests_utils:get("/meeting/testorg/fishy_parameter", Params).

test_list_not_found(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:get("/meeting/unexistentorg/opened", Params).

test_setup(ROOT_UID, ROOT_SID) ->
    Now = utils:now(),
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}
	     , {"start", integer_to_list(Now)}
             , {"metadata[description]", "A new description"}],
    {struct, [{"result", "ok"}]} = tests_utils:post("/meeting/testorg/all/testmeeting", Params),
    {struct, [{"result",
	       {struct,
		[{"org", "testorg"},
		 {"name", "testmeeting"},
		 {"start_date", Now},
		 {"end_date","never"},
		 {"roster",{array, []}},
		 {"metadata",{struct, [{"description", "A new description"}]}}]}}]} = tests_utils:get("/meeting/testorg/all/testmeeting", Params).

test_setup_not_found_meeting(ROOT_UID, ROOT_SID) ->
    Now = integer_to_list(utils:now()),
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}
	     , {"start", Now}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "not_found"}]} = tests_utils:post("/meeting/testorg/all/unexistentmeeting", Params).

test_setup_not_found_org(ROOT_UID, ROOT_SID) ->
    Now = integer_to_list(utils:now()),
    Params = [ {"uid", ROOT_UID}
             , {"sid", ROOT_SID}
	     , {"start", Now}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "not_found"}]} = tests_utils:post("/meeting/unexistentorg/all/testmeeting", Params).

test_join(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],

    {struct, [{"result", "ok"}]} = tests_utils:put("/meeting/testorg/all/testmeeting/roster/" ++ ROOT_UID, Params),

    {struct, [{"result", {array, Array}}]} = tests_utils:get("/meeting/testorg/all/testmeeting/roster", Params),
    [{struct,[{"uid",ROOT_UID},
	      {"auth","password"},
	      {"metadata",{struct,[]}}]}] = Array.

test_join_not_found_meeting(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/meeting/testorg/all/unexistentmeeting/roster/" ++ ROOT_UID, Params).

test_join_not_found_org(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/meeting/unexistentorg/all/testmeeting/roster/" ++ ROOT_UID, Params).

test_join_not_founduid(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/meeting/unexistentorg/all/testmeeting/roster/unexistentuid", Params).

test_leave(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"result", "ok"}]} = tests_utils:delete("/meeting/testorg/all/testmeeting/roster/" ++ ROOT_UID, Params),
    {struct, [{"result", {array, Array}}]} = tests_utils:get("/meeting/testorg/all/testmeeting/roster", Params),
    [] = Array.

test_leave_not_found_meeting(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/meeting/testorg/all/unexistentmeeting/roster/" ++ ROOT_UID, Params).

test_leave_not_found_org(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}
             ],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/meeting/unexistentorg/all/testmeeting/roster/" ++ ROOT_UID, Params).

test_leave_not_founduid(ROOT_UID, ROOT_SID) ->
    Params = [ {"uid", ROOT_UID}
	       , {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/meeting/org/all/testmeeting/roster/unexistentuid", Params).

test_meeting_in_list(Id, {struct, [{"result", {array, List}}]}) ->
    test_meeting_in_list(Id, List);
test_meeting_in_list(Id, []) ->
    throw({not_found, Id});
test_meeting_in_list(Id, [Meeting|Tail]) ->
    {struct,
     [{"org", OrgName},
      {"name", MeetingName},
      {"start_date",_},
      {"end_date",_},
      {"roster",_},
      {"metadata",_}]} = Meeting,
    case Id of
	[OrgName, MeetingName] ->
	    true;
	_ ->
	    test_meeting_in_list(Id, Tail)
    end.

test_meeting_not_in_list(Id, JSON) ->
    case catch test_meeting_in_list(Id, JSON) of
	{not_found, _} ->
	    true;
	_ ->
	    throw({error, found})
    end.
