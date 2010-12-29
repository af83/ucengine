-module(infos_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

event_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(Testers) ->
		[?_test(test_get()),
                ?_test(test_update(Testers)),
                ?_test(test_update_unauthorized(Testers))
                ]
	end}.

-include("mongodb.hrl").

test_get() ->
    {struct, [{"result", {struct, []}}]} = tests_utils:get("/infos", []).

test_update([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
              {"metadata[description]", "UC Engine"}],
    {struct, [{"result", "ok"}]} = tests_utils:post("/infos", Params),
    {struct, [{"result", {struct, [{"description", "UC Engine"}]}}]} = tests_utils:get("/infos", []).

test_update_unauthorized([_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
              {"metadata[description]", "UC Engine"}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:post("/infos", Params).
