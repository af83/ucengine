-module(acl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

acl_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(Testers) ->
	      [  ?_test(test_add(Testers))
		 , ?_test(test_add_not_found(Testers))
		 , ?_test(test_check_true(Testers))
		 , ?_test(test_delete(Testers))
		 , ?_test(test_check_false(Testers))
		 , ?_test(test_delete_not_found(Testers))
	      ]
      end
    }.

test_add([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_add_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/user/unexistentuser/acl/testobject/testaction/testorg/testmeeting", Params).

test_check_true([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "true"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_delete([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", "ok"}]} =
		  tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_check_false([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "c"}],
    {struct, [{"result", "false"}]} =
	tests_utils:get("/user/testuser/acl/event/get/testorg/testmeeting", Params).

test_delete_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/testuser/acl/event/get/testorg/testmeeting", Params).
