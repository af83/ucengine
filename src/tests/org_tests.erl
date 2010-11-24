-module(org_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

org_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(Testers) ->
		[?_test(test_create(Testers)),
		 ?_test(test_create_conflict(Testers)),
		 ?_test(test_create_unauthorized(Testers)),
		 
		 ?_test(test_get(Testers)),
		 ?_test(test_get_not_found(Testers)),
		 
		 ?_test(test_update(Testers)),
		 ?_test(test_update_not_found(Testers)),
		 ?_test(test_update_unauthorized(Testers)),
		 
		 ?_test(test_list(Testers)),
		 ?_test(test_list_unauthorized(Testers))]
      end
    }.

test_create([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "test_org"}],
    {struct,[{"result","created"}]} = tests_utils:put("/org/neworg", Params).

test_create_conflict([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"error", "conflict"}]} = tests_utils:put("/org/neworg", Params).

test_create_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"metadata[description]", "test_org"}],
    {struct,[{"error","unauthorized"}]} = tests_utils:put("/org/neworg", Params).


test_get([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"result",
	      {struct,[{"name","neworg"},
		       {"metadata",{struct,[{"description", "test_org"}]}}
		      ]}}]} = tests_utils:get("/org/neworg", Params).

test_get_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/org/unexistent_org", Params).


test_update([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "test_modified_org"}],
    {struct,[{"result","ok"}]} = tests_utils:post("/org/neworg", Params),
    {struct,[{"result",
	      {struct,[{"name","neworg"},
		       {"metadata",{struct,[{"description", "test_modified_org"}]}}
		      ]}}]} = tests_utils:get("/org/neworg", Params).

test_update_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "test_modified_org"}],
    {struct,[{"error", "not_found"}]} = tests_utils:post("/org/unexistentorg", Params).

test_update_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"metadata[description]", "test_modified_org"}],
    {struct,[{"error", "unauthorized"}]} = tests_utils:post("/org/unexistentorg", Params).


test_list([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"result",
	      {array,
	       [{struct,[{"name",_},
			 {"metadata",_}
			]}|_]}}]} = tests_utils:get("/org/", Params).

test_list_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct,[{"error", "unauthorized"}]} = tests_utils:get("/org/", Params).
