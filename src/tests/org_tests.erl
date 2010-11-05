-module(org_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

org_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(ROOT) ->
	      [?_test(test_create(ROOT)),
	       ?_test(test_create_conflict(ROOT)),

	       ?_test(test_get(ROOT)),
	       ?_test(test_get_not_found(ROOT)),

	       ?_test(test_setup(ROOT)),
	       ?_test(test_setup_not_found(ROOT)),

	       ?_test(test_list(ROOT))
	      ]
      end
    }.

test_create({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
             {"metadata[description]", "test_org"}],
    {struct,[{"result","created"}]} = tests_utils:put("/org/neworg", Params).

test_create_conflict({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct,[{"error", "conflict"}]} = tests_utils:put("/org/neworg", Params).

test_get({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct,[{"result",
	      {struct,[{"name","neworg"},
		       {"metadata",{struct,[{"description", "test_org"}]}}
		      ]}}]} = tests_utils:get("/org/neworg", Params).

test_get_not_found({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/org/unexistent_org", Params).

test_setup({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"metadata[description]", "test_modified_org"}],
    {struct,[{"result","ok"}]} = tests_utils:post("/org/neworg", Params),
    {struct,[{"result",
	      {struct,[{"name","neworg"},
		       {"metadata",{struct,[{"description", "test_modified_org"}]}}
		      ]}}]} = tests_utils:get("/org/neworg", Params).

test_setup_not_found({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"metadata[description]", "test_modified_org"}],
    {struct,[{"error", "not_found"}]} = tests_utils:post("/org/unexistentorg", Params).

test_list({ROOT_UID, ROOT_SID}) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
    {struct,[{"result",
	      {array,
	       [{struct,[{"name",_},
			 {"metadata",_}
			]}|_]}}]} = tests_utils:get("/org/", Params).
