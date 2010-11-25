-module(event_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

event_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(Testers) ->
		[?_test(test_push(Testers)),
		 ?_test(test_push_missing_parameters(Testers)),
		 ?_test(test_push_not_found_org(Testers)),
		 ?_test(test_push_not_found_meeting(Testers)),
		 
		 ?_test(test_get(Testers)),
		 ?_test(test_get_with_keywords(Testers)),
		 ?_test(test_get_with_type_and_timestart(Testers)),
		 ?_test(test_get_with_type_and_timeend(Testers)),
		 ?_test(test_get_with_type_and_timeend_and_org(Testers)),
		 ?_test(test_get_with_timeend_and_from_and_org(Testers)),
		 ?_test(test_last(Testers))]
	end}.

test_push([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params).

test_push_missing_parameters([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:put("/event/testorg/testmeeting", Params).

test_push_not_found_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/event/unexistentorg/testmeeting", Params).

test_push_not_found_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =tests_utils:put("/event/testorg/unexistentmeeting", Params).

test_get([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_2"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_3"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting", Params).

test_get_with_keywords([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "search_event"},
	      {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params),

    ParamsGet = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"search", "lonely"},
		 {"count", "1"}],
    {struct, [{"result", {array,
                          [{struct, [{"type", "search_event"}
				     , {"datetime", _}
				     , {"id", _}
				     , {"org", "testorg"}
				     , {"meeting", "testmeeting"}
				     , {"from", RootUid}
				     , {"metadata", {struct, [{"description", "lonely event"}]}}
				    ]}]}}]} = tests_utils:get("/event/testorg/testmeeting", ParamsGet).

test_get_with_type_and_timestart([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", First}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_2"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_3"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"type", "test_event_3"},
		      {"start", integer_to_list(First + 1)}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_3"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting/", ParamsGetStart).

test_get_with_type_and_timeend([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_2"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_3"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"type", "test_event_2"},
		      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_2"}
				      , {"metadata", {struct, []}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting/", ParamsGetStart).

test_get_with_type_and_timeend_and_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"type", "test_event_2"},
		      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting/", ParamsGetStart).

test_get_with_timeend_and_from_and_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    io:format("~p~n", [tests_utils:get("/event/otherorg/testmeeting", Params)]),
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", First}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_1"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"from", "user_1"},
		      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,
			  [{struct, [{"type", "test_event_1"}
				     , {"datetime", First}
				     , {"id", _}
				     , {"org", "otherorg"}
				     , {"meeting", "testmeeting"}
				     , {"from", "user_1"}
				     , {"metadata", {struct, []}}
				    ]},
			   {struct, [{"type", "test_event_2"}
				     , {"datetime", Second}
				     , {"id", _}
				     , {"org", "otherorg"}
				     , {"meeting", "testmeeting"}
				     , {"from", "user_1"}
				     , {"metadata", {struct, []}}
				    ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting/", ParamsGetStart).

test_last([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "last_event"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params),
    
    ParamsGetLast = [{"uid", RootUid},
		     {"sid", RootSid},
		     {"count", "1"},
		     {"order", "desc"}],
    {struct, [{"result",
	       {array, [{struct, [{"type", "last_event"} 
				  , {"datetime", _}
				  , {"id", _}
				  , {"org", "testorg"}
				  , {"meeting", "testmeeting"}
				  , {"from", _}
				  , {"metadata", {struct, [{"description", "pushed_event"}]}}
				 ]}]}}]} = tests_utils:get("/event/testorg/testmeeting/", ParamsGetLast).
