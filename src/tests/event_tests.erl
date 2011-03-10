%%
%%  U.C.Engine - Unified Colloboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(event_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

setup_events(Domain) ->
    uce_event:add(Domain,
                  #uce_event{ domain=Domain,
                              type="test_event_1",
                              location={"testmeeting", Domain},
                              from={"participant.user@af83.com", Domain}}),
    timer:sleep(10),
    uce_event:add(Domain,
                  #uce_event{ domain=Domain,
                              type="test_event_2",
                              location={"testmeeting", Domain},
                              from={"user_2", Domain}}),
    timer:sleep(10),
    uce_event:add(Domain,
                  #uce_event{ domain=Domain,
                              type="test_event_3",
                              location={"testmeeting", Domain},
                              from={"user_3", Domain},
                              metadata=[{"description", "test"}]}),
    ok.

event_test_() ->
    { setup
      , fun() ->
                [Domain, BaseUrl, Testers] = fixtures:setup(),
                setup_events(Domain),
                [Domain, BaseUrl, Testers]
        end
      , fun fixtures:teardown/1
      , fun([_, BaseUrl, Testers]) ->
                [?_test(test_push(BaseUrl, Testers)),
                 ?_test(test_push_without_meeting(BaseUrl, Testers)),
                 ?_test(test_push_with_parent(BaseUrl, Testers)),
                 ?_test(test_push_to_me(BaseUrl, Testers)),
                 ?_test(test_push_to_other(BaseUrl, Testers)),

                 ?_test(test_push_missing_type(BaseUrl, Testers)),
                 ?_test(test_push_not_found_meeting(BaseUrl, Testers)),
                 ?_test(test_push_not_found_parent(BaseUrl, Testers)),
                 ?_test(test_push_not_found_to(BaseUrl, Testers)),

                 ?_test(test_get(BaseUrl, Testers)),
                 ?_test(test_get_with_keywords(BaseUrl, Testers)),
                 ?_test(test_get_with_keywords_without_meeting(BaseUrl, Testers)),
                 ?_test(test_get_with_keywords_with_from(BaseUrl, Testers)),
                 ?_test(test_get_with_keywords_in_metadata(BaseUrl, Testers)),
                 ?_test(test_get_with_keywords_and_timestart_and_timeend(BaseUrl, Testers)),
                 ?_test(test_get_with_type(BaseUrl, Testers)),
                 ?_test(test_get_with_types(BaseUrl, Testers)),
                 ?_test(test_get_with_type_and_timestart(BaseUrl, Testers)),
                 ?_test(test_get_with_type_and_timestart_and_timeend(BaseUrl, Testers)),
                 ?_test(test_get_with_type_and_timeend(BaseUrl, Testers)),
                 ?_test(test_last(BaseUrl, Testers)),
                 ?_test(test_long_polling(BaseUrl, Testers))]
        end}.

test_push(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),
    ?assertMatch({struct, [{"result",
                            {struct, [{"type", "test_push_1"},
                                      {"domain", _},
                                      {"datetime", _},
                                      {"id", Id},
                                      {"location", "testmeeting"},
                                      {"from", RootUid},
                                      {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, Params)).

test_push_without_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
        tests_utils:post(BaseUrl, "/event/", Params),
    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/all/" ++ Id, Params).

test_push_with_parent(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", ParentId}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsChild = [{"uid", RootUid},
                   {"sid", RootSid},
                   {"type", "test_push_1"},
                   {"parent", ParentId},
                   {"metadata[description]", "pushed_event"}],
    {struct, [{"result", ChildId}]} =
        tests_utils:post(BaseUrl,"/event/testmeeting", ParamsChild),

    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", ChildId},
                         {"location", "testmeeting"},
                         {"from", RootUid},
                         {"parent", ParentId},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ ChildId, Params).

test_push_to_me(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"to", RootUid},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),
    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"location", "testmeeting"},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, Params).

test_push_to_other(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_to_other"},
              {"to", "participant.user@af83.com"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", Id}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, Params),
        ParamsGetStart = [{"uid", RootUid},
                          {"sid", RootSid},
                          {"type", "test_push_to_other"}],
    {struct, [{"result", {array, []}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart).

test_push_missing_type(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_push_not_found_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/unexistentmeeting", Params).

test_push_not_found_parent(BaseUrl, [{RootUid, RootSid}, _]) ->
    ParamsChild = [{"uid", RootUid},
                   {"sid", RootSid},
                   {"type", "test_push_1"},
                   {"parent", "unexistent_id"},
                   {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", ParamsChild).

test_push_not_found_to(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"to", "unexistent_user"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_get(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],

    ?assertMatch({struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]}, tests_utils:get(BaseUrl, "/event/testmeeting", Params)).

test_get_with_keywords(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    timer:sleep(1000),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"search", "lonely"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "search_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "lonely event"}]}}
                                    ]}]}}]}, 
                 tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet)).

test_get_with_keywords_without_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely hungry event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"search", "hungry"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "search_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "lonely hungry event"}]}}
                                    ]}]}}]}, tests_utils:get(BaseUrl, "/event/", ParamsGet)).

test_get_with_keywords_with_from(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    timer:sleep(1000),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"from", RootUid},
                 {"search", "lonely"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "search_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "lonely event"}]}}
                                    ]}]}}]}, 
                 tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet)).

test_get_with_keywords_in_metadata(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely happy event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),
    
    timer:sleep(1000),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"search", "happy"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                                       [{struct, [{"type", "search_event"}
                                                  , {"domain", _}
                                                  , {"datetime", _}
                                                  , {"id", _}
                                                  , {"location", "testmeeting"}
                                                  , {"from", RootUid}
                                                  , {"metadata", {struct, [{"description", "lonely happy event"}]}}
                                                 ]}]}}]}, 
                 tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet)).

test_get_with_keywords_and_timestart_and_timeend(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),

    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3"},
                      {"search", "test"},
                      {"start", integer_to_list(Third)},
                      {"end", integer_to_list(Third + 1)}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart),
    ParamsGetNothing = [{"uid", RootUid},
                        {"sid", RootSid},
                        {"type", "test_event_3"},
                        {"search", "test"},
                        {"start", integer_to_list(Third - 2)},
                        {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,[]}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetNothing).

test_get_with_type(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3"}],
    ?assertMatch({struct, [{"result", {array,
                                       [ {struct, [{"type", "test_event_3"}
                                                   , {"domain", _}
                                                   , {"datetime", Third}
                                                   , {"id", _}
                                                   , {"location", "testmeeting"}
                                                   , {"from", "user_3"}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_types(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3,test_event_1"}],
    ?assertMatch({struct, [{"result", {array,
                                       [ {struct, [{"type", "test_event_1"}
                                                   , {"domain", _}
                                                   , {"datetime", _}
                                                   , {"id", _}
                                                   , {"location", "testmeeting"}
                                                   , {"from", "participant.user@af83.com"}
                                                   , {"metadata", {struct, []}}
                                                  ]},
                                         {struct, [{"type", "test_event_3"}
                                                   , {"domain", _}
                                                   , {"datetime", Third}
                                                   , {"id", _}
                                                   , {"location", "testmeeting"}
                                                   , {"from", "user_3"}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_type_and_timestart(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3"},
                      {"start", integer_to_list(Third)}],
    ?assertMatch({struct, [{"result", {array,
                                       [ {struct, [{"type", "test_event_3"}
                                                   , {"domain", _}
                                                   , {"datetime", Third}
                                                   , {"id", _}
                                                   , {"location", "testmeeting"}
                                                   , {"from", "user_3"}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_type_and_timestart_and_timeend(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3"},
                      {"start", integer_to_list(Third)},
                      {"end", integer_to_list(Third + 1)}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart),
    ParamsGetNothing = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_3"},
                      {"start", integer_to_list(Third - 2)},
                      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,[]}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetNothing).

test_get_with_type_and_timeend(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "participant.user@af83.com"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", Second}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_3"}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting", Params),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"type", "test_event_2"},
                      {"end", integer_to_list(Third - 1)}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", Second}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", "user_2"}
                                      , {"metadata", {struct, []}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart).

test_last(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "last_event"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsGetLast = [{"uid", RootUid},
                     {"sid", RootSid},
                     {"count", "1"},
                     {"order", "desc"}],
    {struct, [{"result",
               {array, [{struct, [{"type", "last_event"}
                                  , {"domain", _}
                                  , {"datetime", _}
                                  , {"id", _}
                                  , {"location", "testmeeting"}
                                  , {"from", _}
                                  , {"metadata", {struct, [{"description", "pushed_event"}]}}
                                 ]}]}}]} = tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetLast).

send_long_polling_event(BaseUrl, {RootUid, RootSid}) ->
    timer:sleep(4000),
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "long_polling_event"},
              {"metadata[description]", "relax, don't do it"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_long_polling(BaseUrl, [{RootUid, RootSid}, _]) ->
    Now = utils:now(),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"start", integer_to_list(Now)},
                 {"type", "long_polling_event"},
                 {"_async", "lp"}],
    spawn(?MODULE, send_long_polling_event, [BaseUrl, {RootUid, RootSid}]),
    {struct, [{"result", {array,
                          [{struct, [{"type", "long_polling_event"}
                                     , {"domain", _}
                                     , {"datetime", Datetime}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "relax, don't do it"}]}}
                                    ]}]}}]} = tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet),
    LongPollingDelay = utils:now() - Now,
    EventDelay = Datetime - Now,
    % both should be around 2000
    if
        LongPollingDelay < 3700 ->
            throw({error, too_fast});
        LongPollingDelay > 4700 ->
            throw({error, too_much_delay});
        true ->
            nothing
    end,
    if
        EventDelay < 3700 ->
            throw({error, too_fast});
        EventDelay > 4700 ->
            throw({error, too_much_delay});
        true ->
            nothing
    end.
