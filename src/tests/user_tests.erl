-module(user_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

user_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun({ROOT_UID, ROOT_SID}) ->
        [ ?_test(test_register())
        , ?_test(test_register_conflict())
	 
        , ?_test(test_get(ROOT_UID, ROOT_SID))
	  , ?_test(test_get_not_found(ROOT_UID, ROOT_SID))

        , ?_test(test_list(ROOT_UID, ROOT_SID))

        , ?_test(test_update(ROOT_UID, ROOT_SID))
	  , ?_test(test_update_not_found(ROOT_UID, ROOT_SID))

        , ?_test(test_delete(ROOT_UID, ROOT_SID))
        ]
      end
    }.

test_register() ->
    Params = [{"auth", "test"},
	      {"credential", "test"},
	      {"metadata[nickname]", "test_nickname"}],
    {struct, [{"result", "created"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_register_conflict() ->
    Params = [{"auth", "test"},
	      {"credential", "test"}],
    {struct, [{"error", "conflict"}]} = tests_utils:put("/user/test.user@af83.com", Params).

test_get(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct,[{"result",
	      {struct,[{"uid","test.user@af83.com"},
		       {"auth","test"},
		       {"metadata",{struct,[{"nickname", "test_nickname"}]}}
		      ]}
	     }]} = tests_utils:get("/user/test.user@af83.com", Params).

test_get_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct, [{"error", "not_found"}]} = tests_utils:get("/user/unexistent.user@af83.com", Params).

test_list(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
             {"sid", ROOT_SID}],
    {struct,[{"result",
	      {array,
	       [{struct,[{"uid",_},
			 {"auth",_},
			 {"metadata",{struct,_}}
			 ]}|_]
	     }}]} = tests_utils:get("/user/", Params).

test_update(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"auth", "test_modified"},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"result", "ok"}]} = tests_utils:post("/user/test.user@af83.com", Params).

test_update_not_found(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"auth", "test_modified"},
	      {"credential", "test_modified"},
	      {"metadata[nickname]", "test_modified_nickname"}],
    {struct, [{"error", "not_found"}]} = tests_utils:post("/user/unexistent.user@af83.com", Params).

test_delete(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct, [{"result", "ok"}]} = tests_utils:delete("/user/test.user@af83.com", Params),
    {struct, [{"error", "not_found"}]} = tests_utils:get("/user/test.user@af83.com", Params).
