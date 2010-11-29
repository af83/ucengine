-module(event_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

event_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun(Testers) ->
		[?_test(test_push(Testers)),
		 ?_test(test_push_without_meeting(Testers)),
		 ?_test(test_push_without_org(Testers)),
		 ?_test(test_push_with_parent(Testers)),
		 ?_test(test_push_to_me(Testers)),
		 ?_test(test_push_to_other(Testers)),

		 ?_test(test_push_missing_type(Testers)),
		 ?_test(test_push_not_found_org(Testers)),
		 ?_test(test_push_not_found_meeting(Testers)),
		 ?_test(test_push_not_found_parent(Testers)),
		 ?_test(test_push_not_found_to(Testers)),

		 ?_test(test_get(Testers)),
		 ?_test(test_get_with_keywords(Testers)),
		 ?_test(test_get_with_type(Testers)),
		 ?_test(test_get_with_types(Testers)),
		 ?_test(test_get_with_type_and_timestart(Testers)),
		 ?_test(test_get_with_type_and_timestart_and_timeend(Testers)),
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
    {struct, [{"result", Id}]} =
	tests_utils:put("/event/testorg/testmeeting", Params),
    {struct, [{"result",
	       {struct, [{"type", "test_event_1"},
			 {"datetime", _},
			 {"id", Id},
			 {"org", "testorg"},
			 {"meeting", "testmeeting"},
			 {"from", RootUid},
			 {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
		   tests_utils:get("/event/testorg/testmeeting/" ++ Id, Params).

test_push_without_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
	tests_utils:put("/event/testorg/", Params),
    {struct, [{"result",
	       {struct, [{"type", "test_event_1"},
			 {"datetime", _},
			 {"id", Id},
			 {"org", "testorg"},
			 {"from", RootUid},
			 {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
		   tests_utils:get("/event/testorg/all/" ++ Id, Params).

test_push_without_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
	tests_utils:put("/event/", Params),
    {struct, [{"result",
	       {struct, [{"type", "test_event_1"},
			 {"datetime", _},
			 {"id", Id},
			 {"from", RootUid},
			 {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
		   tests_utils:get("/event/all/all/" ++ Id, Params).

test_push_with_parent([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", ParentId}]} =
	tests_utils:put("/event/testorg/testmeeting", Params),

    ParamsChild = [{"uid", RootUid},
		   {"sid", RootSid},
		   {"type", "test_event_1"},
		   {"parent", ParentId},
		   {"metadata[description]", "pushed_event"}],
    {struct, [{"result", ChildId}]} =
	tests_utils:put("/event/testorg/testmeeting", ParamsChild),

    {struct, [{"result",
	       {struct, [{"type", "test_event_1"},
			 {"datetime", _},
			 {"id", ChildId},
			 {"org", "testorg"},
			 {"meeting", "testmeeting"},
			 {"from", RootUid},
			 {"parent", ParentId},
			 {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
		   tests_utils:get("/event/testorg/testmeeting/" ++ ChildId, Params).

test_push_to_me([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"to", RootUid},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
	tests_utils:put("/event/testorg/testmeeting", Params),
    {struct, [{"result",
	       {struct, [{"type", "test_event_1"},
			 {"datetime", _},
			 {"id", Id},
			 {"org", "testorg"},
			 {"meeting", "testmeeting"},
			 {"from", RootUid},
			 {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
		   tests_utils:get("/event/testorg/testmeeting/" ++ Id, Params).

test_push_to_other([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_to_other"},
	      {"to", "participant.user@af83.com"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
	tests_utils:put("/event/testorg/testmeeting", Params),
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:get("/event/testorg/testmeeting/" ++ Id, Params),
        ParamsGetStart = [{"uid", RootUid},
			  {"sid", RootSid},
			  {"type", "test_event_to_other"}],
    {struct, [{"result", {array, []}}]} =
	tests_utils:get("/event/testorg/testmeeting/", ParamsGetStart).

test_push_missing_type([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}]} =
	tests_utils:put("/event/testorg/testmeeting", Params).

test_push_not_found_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/event/unexistentorg/testmeeting", Params).

test_push_not_found_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/event/testorg/unexistentmeeting", Params).

test_push_not_found_parent([{RootUid, RootSid}, _]) ->
    ParamsChild = [{"uid", RootUid},
		   {"sid", RootSid},
		   {"type", "test_event_1"},
		   {"parent", "unexistent_id"},
		   {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/event/testorg/testmeeting", ParamsChild).   

test_push_not_found_to([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"type", "test_event_1"},
	      {"to", "unexistent_user"},
	      {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put("/event/testorg/testmeeting", Params).

test_get([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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

test_get_with_type([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
		      {"type", "test_event_3"}],
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

test_get_with_types([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
		      {"type", "test_event_3,test_event_1"}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
			 }}]} = tests_utils:get("/event/testorg/testmeeting/", ParamsGetStart).

test_get_with_type_and_timestart([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
		      {"start", integer_to_list(Third)}],
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

test_get_with_type_and_timestart_and_timeend([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
		      {"start", integer_to_list(Third)},
		      {"end", integer_to_list(Third + 1)}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "user_3"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/testorg/testmeeting/", ParamsGetStart),
    ParamsGetNothing = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"type", "test_event_3"},
		      {"start", integer_to_list(Third - 2)},
		      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,[]}}]} =
	tests_utils:get("/event/testorg/testmeeting/", ParamsGetNothing).

test_get_with_type_and_timeend([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", _}
				      , {"id", _}
				      , {"org", "testorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
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
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, []}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting/", ParamsGetStart).

test_get_with_timeend_and_from_and_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"result", {array,
			  [ {struct, [{"type", "test_event_1"}
				      , {"datetime", First}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_2"}
				      , {"datetime", Second}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, []}}
				     ]},
			    {struct, [{"type", "test_event_3"}
				      , {"datetime", Third}
				      , {"id", _}
				      , {"org", "otherorg"}
				      , {"meeting", "testmeeting"}
				      , {"from", "participant.user@af83.com"}
				      , {"metadata", {struct, [{"description", "test"}]}}
				     ]}|_]
			 }}]} = tests_utils:get("/event/otherorg/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
		      {"sid", RootSid},
		      {"from", "participant.user@af83.com"},
		      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,
			  [{struct, [{"type", "test_event_1"}
				     , {"datetime", First}
				     , {"id", _}
				     , {"org", "otherorg"}
				     , {"meeting", "testmeeting"}
				     , {"from", "participant.user@af83.com"}
				     , {"metadata", {struct, []}}
				    ]},
			   {struct, [{"type", "test_event_2"}
				     , {"datetime", Second}
				     , {"id", _}
				     , {"org", "otherorg"}
				     , {"meeting", "testmeeting"}
				     , {"from", "participant.user@af83.com"}
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
