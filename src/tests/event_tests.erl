-module(event_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

event_test_() ->
	{ setup
	  , fun fixtures:setup/0
	  , fun fixtures:teardown/1
	  , fun({ROOT_UID, ROOT_SID}) ->
				[?_test(test_push(ROOT_UID, ROOT_SID)),
				 ?_test(test_push_missing_parameters(ROOT_UID, ROOT_SID)),
				 ?_test(test_push_not_found_org(ROOT_UID, ROOT_SID)),
				 ?_test(test_push_not_found_meeting(ROOT_UID, ROOT_SID)),
				   
				 ?_test(test_get(ROOT_UID, ROOT_SID)),
				 ?_test(test_get_with_keywords(ROOT_UID, ROOT_SID)),
				 ?_test(test_get_with_type_and_timestart(ROOT_UID, ROOT_SID)),
				 ?_test(test_get_with_type_and_timeend(ROOT_UID, ROOT_SID)),
				 ?_test(test_get_with_type_and_timeend_and_org(ROOT_UID, ROOT_SID)),
				 ?_test(test_get_with_timeend_and_from_and_org(ROOT_UID, ROOT_SID)),
				 ?_test(test_last(ROOT_UID, ROOT_SID))]
	  end
	}.

test_push(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params).

test_push_missing_parameters(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}]} = tests_utils:put("/event/testorg/testmeeting", Params).

test_push_not_found_org(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} = tests_utils:put("/event/unexistentorg/testmeeting", Params).

test_push_not_found_meeting(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =tests_utils:put("/event/testorg/unexistentmeeting", Params).

test_get(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
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

test_get_with_keywords(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"type", "search_event"},
	      {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params),

    ParamsGet = [{"uid", ROOT_UID},
		 {"sid", ROOT_SID},
		 {"search", "lonely"},
		 {"count", "1"}],
    ?DEBUG("~p~n", [tests_utils:get("/event/testorg/testmeeting", ParamsGet)]),
    {struct, [{"result", {array,
                          [{array, [{struct, [{"type", "search_event"}
				    , {"datetime", _}
				    , {"id", _}
				    , {"org", "testorg"}
				    , {"meeting", "testmeeting"}
				    , {"from", ROOT_UID}
				    , {"metadata", {struct, [{"description", "lonely event"}]}}
				   ]}]}]}}]} = tests_utils:get("/event/testorg/testmeeting", ParamsGet).

test_get_with_type_and_timestart(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
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
    ParamsGetStart = [{"uid", ROOT_UID},
		      {"sid", ROOT_SID},
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

test_get_with_type_and_timeend(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
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
    ParamsGetStart = [{"uid", ROOT_UID},
		      {"sid", ROOT_SID},
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

test_get_with_type_and_timeend_and_org(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
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
    ParamsGetStart = [{"uid", ROOT_UID},
		      {"sid", ROOT_SID},
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

test_get_with_timeend_and_from_and_org(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID}],
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
    ParamsGetStart = [{"uid", ROOT_UID},
		      {"sid", ROOT_SID},
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

test_last(ROOT_UID, ROOT_SID) ->
    Params = [{"uid", ROOT_UID},
	      {"sid", ROOT_SID},
	      {"type", "last_event"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:put("/event/testorg/testmeeting", Params),
    
    ParamsGetLast = [{"uid", ROOT_UID},
		     {"sid", ROOT_SID},
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
