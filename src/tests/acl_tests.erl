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
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_add_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/user/unexistentuser/acl/testobject/testaction/testorg/testmeeting", Params).

test_check_true(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "true"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_delete(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct, [{"result", "ok"}]} =
		  tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_check_false(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[a]", "c"}],
    {struct, [{"result", "false"}]} =
	tests_utils:get("/user/testuser/acl/event/get/testorg/testmeeting", Params).

test_delete_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/testuser/acl/event/get/testorg/testmeeting", Params).
