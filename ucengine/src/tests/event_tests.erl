%%
%%  U.C.Engine - Unified Collaboration Engine
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

-export([send_long_polling_event/2]).


setup_events(Domain) ->
    {ok, Participant} = uce_user:get_by_name(Domain, "participant.user@af83.com"),
    {ok, User2} = uce_user:get_by_name(Domain, "user_2"),
    {ok, User3} = uce_user:get_by_name(Domain, "user_3"),

    uce_event:add(Domain,
                  #uce_event{ id=none,
                              type="test_event_1",
                              location="testmeeting",
                              from=Participant#uce_user.id}),
    timer:sleep(10),
    uce_event:add(Domain,
                  #uce_event{ id=none,
                              type="test_event_2",
                              location="testmeeting",
                              from=User2#uce_user.id}),
    timer:sleep(10),
    uce_event:add(Domain,
                  #uce_event{ id=none,
                              type="test_event_3",
                              location="testmeeting",
                              from=User3#uce_user.id,
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
      , fun([_, BaseUrl, [Root, Participant, Ugly|_]]) ->
                [?_test(test_push(BaseUrl, Root)),
                 ?_test(test_push_big(BaseUrl, Root)),
                 ?_test(test_push_internal_event(BaseUrl, Root)),
                 ?_test(test_push_without_meeting(BaseUrl, Root)),
                 ?_test(test_push_with_parent(BaseUrl, Root)),
                 ?_test(test_push_to_me(BaseUrl, Root)),
                 ?_test(test_push_to_unauthorized(BaseUrl, Root, Participant, Ugly)),
                 ?_test(test_push_to_other(BaseUrl, Root, Participant)),

                 ?_test(test_push_missing_type(BaseUrl, Root)),
                 ?_test(test_push_not_found_meeting(BaseUrl, Root)),
                 ?_test(test_push_not_found_parent(BaseUrl, Root)),
                 ?_test(test_push_not_found_to(BaseUrl, Root)),

                 ?_test(test_get(BaseUrl, Root)),
                 ?_test(test_get_without_meeting(BaseUrl, Root)),
                 ?_test(test_get_with_keywords(BaseUrl, Root)),
                 ?_test(test_get_with_keywords_without_meeting(BaseUrl, Root)),
                 ?_test(test_get_with_keywords_with_from(BaseUrl, Root, Participant)),
                 ?_test(test_get_with_keywords_in_metadata(BaseUrl, Root)),
                 ?_test(test_get_with_keywords_and_timestart_and_timeend(BaseUrl, Root)),
                 ?_test(test_get_with_type(BaseUrl, Root)),
                 ?_test(test_get_with_types(BaseUrl, Root)),
                 ?_test(test_get_with_type_and_timestart(BaseUrl, Root)),
                 ?_test(test_get_with_type_and_timestart_and_timeend(BaseUrl, Root)),
                 ?_test(test_get_with_type_and_timeend(BaseUrl, Root)),
                 ?_test(test_last(BaseUrl, Root)),
                 ?_test(test_long_polling(BaseUrl, Root))]
        end}.

test_push(BaseUrl, {RootUid, RootSid}) ->
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

test_push_big(BaseUrl, {RootUid, RootSid}) ->
    Text = string:copies("pushed_event", 5000),
    BaseParams = [{"uid", RootUid},
                  {"sid", RootSid}],
    Params = BaseParams ++ [{"type", "test_push_1"},
                            {"metadata[description]", Text}],
    {struct, [{"result", Id}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),
    ?assertMatch({struct, [{"result",
                            {struct, [{"type", "test_push_1"},
                                      {"domain", _},
                                      {"datetime", _},
                                      {"id", Id},
                                      {"location", "testmeeting"},
                                      {"from", RootUid},
                                      {"metadata", {struct, [{"description", Text}]}}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, BaseParams)).

test_push_internal_event(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "internal.roster.add"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_push_without_meeting(BaseUrl, {RootUid, RootSid}) ->
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

test_push_with_parent(BaseUrl, {RootUid, RootSid}) ->
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

test_push_to_me(BaseUrl, {RootUid, RootSid}) ->
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
                         {"to", RootUid},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, Params).

test_push_to_unauthorized(BaseUrl,
                          {RootUid, RootSid},
                          {ParticipantUid, ParticipantSid},
                          {UglyUid, _UglySid}) ->
    Params = [{"uid", ParticipantUid},
              {"sid", ParticipantSid},
              {"type", "test_push_1"},
              {"to", UglyUid},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _Id}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsRoot = [{"uid", RootUid},
                  {"sid", RootSid},
                  {"type", "test_push_to_other"}],
     {struct, [{"result", {array, []}}]} =
         tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsRoot).

test_push_to_other(BaseUrl, {RootUid, RootSid}, {ParticipantUid, ParticipantSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_to_other"},
              {"to", ParticipantUid},
              {"metadata[description]", "pushed_event"}],

    {struct, [{"result", Id}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    {struct, [{"error", "unauthorized"}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, Params),

    ParamsParticipant = [{"uid", ParticipantUid},
                         {"sid", ParticipantSid},
                         {"type", "test_push_to_other"}],
    {struct, [{"result", {array, [
               {struct, [{"type", "test_push_to_other"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"location", "testmeeting"},
                         {"to", "participant.user@af83.com"},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}]}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsParticipant),

    ParamsRootGet = [{"uid", RootUid},
                     {"sid", RootSid},
                     {"type", "test_push_to_other"}],
    {struct, [{"result", {array, [
               {struct, [{"type", "test_push_to_other"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"location", "testmeeting"},
                         {"to", "participant.user@af83.com"},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}]}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsRootGet).

test_push_missing_type(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_push_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/unexistentmeeting", Params).

test_push_not_found_parent(BaseUrl, {RootUid, RootSid}) ->
    ParamsChild = [{"uid", RootUid},
                   {"sid", RootSid},
                   {"type", "test_push_1"},
                   {"parent", "unexistent_id"},
                   {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", ParamsChild).

test_push_not_found_to(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "test_push_1"},
              {"to", "unexistent_user"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_get(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],

    ?assertMatch({struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, [{"description", "test"}]}}
                                     ]}|_]
                         }}]}, tests_utils:get(BaseUrl, "/event/testmeeting", Params)).

test_get_without_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "non_global_event"},
              {"metadata[description]", "plop2"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    Params2 = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "global_event"},
              {"metadata[description]", "plop"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event", Params2),

    timer:sleep(1000),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"order", "desc"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "global_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "plop"}]}}
                                    ]}]}}]},
                 tests_utils:get(BaseUrl, "/event/", ParamsGet)),

    ParamsGetMeeting = [{"uid", RootUid},
                        {"sid", RootSid},
                        {"order", "desc"},
                        {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "non_global_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "plop2"}]}}
                                    ]}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGetMeeting)).

test_get_with_keywords(BaseUrl,  {RootUid, RootSid}) ->
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

test_get_with_keywords_without_meeting(BaseUrl, {RootUid, RootSid}) ->
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

test_get_with_keywords_with_from(BaseUrl, {RootUid, RootSid}, {ParticipantUid, ParticipantSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event_test_from"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    Params2 = [{"uid", ParticipantUid},
              {"sid", ParticipantSid},
              {"type", "search_event_test_from"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params2),

    timer:sleep(1000),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"from", RootUid},
                 {"search", "lonely"},
                 {"order", "desc"},
                 {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "search_event_test_from"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "lonely event"}]}}
                                    ]}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet)).

test_get_with_keywords_in_metadata(BaseUrl, {RootUid, RootSid}) ->
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

test_get_with_keywords_and_timestart_and_timeend(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                      , {"from", _}
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

test_get_with_type(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                                   , {"from", _}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_types(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                                   , {"from", _}
                                                   , {"metadata", {struct, []}}
                                                  ]},
                                         {struct, [{"type", "test_event_3"}
                                                   , {"domain", _}
                                                   , {"datetime", Third}
                                                   , {"id", _}
                                                   , {"location", "testmeeting"}
                                                   , {"from", _}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_type_and_timestart(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                                   , {"from", _}
                                                   , {"metadata", {struct, [{"description", "test"}]}}
                                                  ]}|_]
                                      }}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart)).

test_get_with_type_and_timestart_and_timeend(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                      , {"from", _}
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

test_get_with_type_and_timeend(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {array,
                          [ {struct, [{"type", "test_event_1"}
                                      , {"domain", _}
                                      , {"datetime", _}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_2"}
                                      , {"domain", _}
                                      , {"datetime", Second}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]},
                            {struct, [{"type", "test_event_3"}
                                      , {"domain", _}
                                      , {"datetime", Third}
                                      , {"id", _}
                                      , {"location", "testmeeting"}
                                      , {"from", _}
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
                                      , {"from", _}
                                      , {"metadata", {struct, []}}
                                     ]}|_]
                         }}]} = tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetStart).

test_last(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "last_event"},
              {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsGetLast = [{"uid", RootUid},
                     {"sid", RootSid},
                     {"count", "1"},
                     {"order", "desc"}],
    ?assertMatch({struct, [{"result",
                            {array, [{struct, [{"type", "last_event"}
                                  , {"domain", _}
                                  , {"datetime", _}
                                  , {"id", _}
                                  , {"location", "testmeeting"}
                                  , {"from", _}
                                  , {"metadata", {struct, [{"description", "pushed_event"}]}}
                                 ]}]}}]}, tests_utils:get(BaseUrl, "/event/testmeeting/", ParamsGetLast)).

send_long_polling_event(BaseUrl, {RootUid, RootSid}) ->
    timer:sleep(4000),
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "long_polling_event"},
              {"metadata[description]", "relax, don't do it"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_long_polling(BaseUrl, {RootUid, RootSid}) ->
    Now = utils:now(),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"start", integer_to_list(Now)},
                 {"type", "long_polling_event"},
                 {"mode", "longpolling"}],
    spawn(?MODULE, send_long_polling_event, [BaseUrl, {RootUid, RootSid}]),
    {struct, [{"result", {array,
                          [{struct, [{"type", "long_polling_event"}
                                     , {"domain", _}
                                     , {"datetime", Datetime}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "relax, don't do it"}]}}
                                    ]}]}}]} = tests_utils:get(BaseUrl, "/live/testmeeting", ParamsGet),
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
