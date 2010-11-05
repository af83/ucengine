-module(acl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

acl_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun({ROOT_UID, ROOT_SID}) ->
	      [  ?_test(test_add(ROOT_UID, ROOT_SID))
		 , ?_test(test_add_not_found(ROOT_UID, ROOT_SID))
		 , ?_test(test_check_true(ROOT_UID, ROOT_SID))
		 , ?_test(test_delete(ROOT_UID, ROOT_SID))
		 , ?_test(test_check_false(ROOT_UID, ROOT_SID))
		 , ?_test(test_delete_not_found(ROOT_UID, ROOT_SID))
	      ]
      end
    }.

test_add(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[org]", "testorg"},
	      {"conditions[meeting]", "testmeeting"}],
    {struct, [{"result", "created"}]} = tests_utils:put("/user/participant.user@af83.com/acl/get/event", Params).

test_add_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[org]", "testaclorg"},
	      {"conditions[meeting]", "testaclmeeting"}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/user/unexistentuser/acl/testaction/testobject", Params).

test_check_true(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[org]", "testorg"},
	      {"conditions[meeting]", "testmeeting"}],
    {struct, [{"result", "true"}]} = tests_utils:get("/user/participant.user@af83.com/acl/get/event", Params).

test_delete(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[org]", "testorg"},
	      {"conditions[meeting]", "testmeeting"}],
    {struct, [{"result", "ok"}]} = tests_utils:delete("/user/participant.user@af83.com/acl/get/event", Params).

test_check_false(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[org]", "testorg"},
	      {"conditions[meeting]", "testmeeting"}],
    {struct, [{"result", "false"}]} = tests_utils:get("/user/testuser/acl/get/event", Params).

test_delete_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"action", "testaction"},
	      {"object", "testobject"}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/acl/unexistentuser", Params).
