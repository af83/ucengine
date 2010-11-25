-module(acl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

acl_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(Testers) ->
	      [?_test(test_add(Testers)),
	       ?_test(test_add_not_found_user(Testers)),
	       ?_test(test_add_not_found_org(Testers)),
	       ?_test(test_add_not_found_meeting(Testers)),
	       ?_test(test_add_unauthorized(Testers)),

	       ?_test(test_check_false_location(Testers)),
	       ?_test(test_check_false_location_without_meeting(Testers)),
	       ?_test(test_check_false_location_without_org(Testers)),
	       ?_test(test_check_false_conditions(Testers)),

	       ?_test(test_check_unauthorized(Testers)),

	       ?_test(test_check_true(Testers)),
	       ?_test(test_check_true_without_meeting(Testers)),
	       ?_test(test_check_true_without_org(Testers)),

	       ?_test(test_delete(Testers)),
	       ?_test(test_delete_unauthorized(Testers)),
	       ?_test(test_delete_not_found_meeting(Testers)),
	       ?_test(test_delete_not_found_org(Testers)),
	       ?_test(test_delete_not_found_action(Testers)),
	       ?_test(test_delete_not_found_object(Testers)),
	       ?_test(test_delete_not_found_user(Testers)),
	       ?_test(test_delete_not_found_conditions(Testers))]
      end
    }.

test_add([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_add_not_found_user([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/user/unexistentuser/acl/testobject/testaction/testorg/testmeeting", Params).

test_add_not_found_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/user/participant.user@af83.com/acl/testobject/testaction/unexistentorg/testmeeting", Params).

test_add_not_found_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/user/participant.user@af83.com/acl/testobject/testaction/org/unexistentmeeting", Params).

test_add_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:put("/user/unexistentuser/acl/testobject/testaction/testorg/testmeeting", Params).

test_check_true([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "true"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_check_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_check_true_without_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/testobject/testaction/testorg/", Params),
    {struct, [{"result", "true"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/testobject/testaction/testorg/testmeeting", Params).

test_check_true_without_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/testobject2/testaction2/", Params),
    {struct, [{"result", "true"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/testobject2/testaction2/testorg/testmeeting", Params).

test_check_false_location([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/othermeeting", Params).

test_check_false_location_without_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/", Params).

test_check_false_location_without_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/", Params).

test_check_false_conditions([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "c"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).

test_delete([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "ok"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params),
    ParamsCheck = [{"uid", RootUid},
		   {"sid", RootSid},
		   {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} =
	tests_utils:get("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsCheck),
    ParamsDelete = [{"uid", RootUid},
		    {"sid", RootSid},
		    {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsDelete).


test_delete_unauthorized([{RootUid, RootSid}, {UglyUid, UglySid}]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/unexistentmeeting", Params).
    
test_delete_not_found_meeting([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/unexistentmeeting", Params).

test_delete_not_found_org([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/unexistentorg/testmeeting", Params).

test_delete_not_found_action([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/unexistentaction/testorg/testmeeting", Params).

test_delete_not_found_object([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/unexistentobject/get/testorg/testmeeting", Params).

test_delete_not_found_user([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/unexistentuser/acl/event/get/testorg/testmeeting", Params).

test_delete_not_found_conditions([{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "c"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/user/participant.user@af83.com/acl/event/get/testorg/testmeeting", Params).
