-module(user_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

user_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(Testers) ->
        [?_test(test_register()),
	 ?_test(test_register_missing_auth()),
	 ?_test(test_register_missing_credential()),
	 ?_test(test_register_conflict()),
	 
	 ?_test(test_get(Testers)),
	 ?_test(test_get_not_found(Testers)),
	 ?_test(test_get_unauthorized(Testers)),

	 ?_test(test_list(Testers)),
	 ?_test(test_list_unauthorized(Testers)),

	 ?_test(test_update(Testers)),
	 ?_test(test_update_missing_auth(Testers)),
	 ?_test(test_update_missing_credential(Testers)),
	 ?_test(test_update_not_found(Testers)),
	 ?_test(test_update_unauthorized(Testers)),

	 ?_test(test_delete_unauthorized(Testers)),
	 ?_test(test_delete(Testers)),
	 ?_test(test_delete_not_found(Testers))
        ]
      end
    }.

test_register() ->
    Params = [{"auth", "test"},
	      {"credential", "test"},
	      {"metadata[nickname]", "test_nickname"}],
    {struct, [{"result", "created"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_register_missing_auth() ->
    Params = [{"credential", "test"},
	      {"metadata[nickname]", "test_nickname"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_register_missing_credential() ->
    Params = [{"auth", "test"},
	      {"metadata[nickname]", "test_nickname"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_register_conflict() ->
    Params = [{"auth", "test"},
	      {"credential", "test"}],
    {struct, [{"error", "conflict"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_get([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"result",
	      {struct,[{"uid","test.user@af83.com"},
		       {"auth","test"},
		       {"metadata",{struct,[{"nickname", "test_nickname"}]}}
		      ]}
	     }]} = tests_utils:get("/user/test.user@af83.com", Params).

test_get_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} = tests_utils:get("/user/unexistent.user@af83.com", Params).

test_get_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:get("/user/unexistent.user@af83.com", Params).

test_list([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
             {"sid", RootSid}],
    {struct,[{"result",
	      {array,
	       [{struct,[{"uid",_},
			 {"auth",_},
			 {"metadata",{struct,_}}
			 ]}|_]
	     }}]} = tests_utils:get("/user/", Params).

test_list_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:get("/user/", Params).

test_update([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"auth", "test_modified"},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"result", "ok"}]} = tests_utils:post("/user/test.user@af83.com", Params).


test_update_missing_auth([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:post("/user/test.user@af83.com", Params).

test_update_missing_credential([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"auth", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:post("/user/test.user@af83.com", Params).

test_update_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"auth", "test_modified"},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"error", "not_found"}]} = tests_utils:post("/user/unexistent.user@af83.com", Params).

test_update_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"auth", "test_modified"},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:post("/user/test.user@af83.com", Params).


test_delete_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:delete("/user/test.user@af83.com", Params).

test_delete([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", "ok"}]} = tests_utils:delete("/user/test.user@af83.com", Params),
    {struct, [{"error", "not_found"}]} = tests_utils:get("/user/test.user@af83.com", Params).

test_delete_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/user/unexistent.user@af83.com", Params).
