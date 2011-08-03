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

-export([send_long_polling_event/2, send_long_polling_event/3]).

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
                              metadata=json_helpers:to_struct([{"description", "test"}])}),
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
                 ?_test(test_push_json(BaseUrl, Root)),

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
                 ?_test(test_long_polling(BaseUrl, Root)),
                 ?_test(test_long_polling_with_types(BaseUrl, Root))
                ]
        end}.

event_timeout_test_() ->
    { setup
      , fun() ->
                [Domain, BaseUrl, Testers] = fixtures:setup(),
                setup_events(Domain),
                % Set the timeout to a low value to avoid waiting too long.
                PreviousTimeout = config:get(connection_timeout),
                config:set(connection_timeout, 10),
                [Domain, BaseUrl, Testers, PreviousTimeout]
        end
      , fun ([Domain, BaseUrl, Testers, PreviousTimeout]) ->
                %% Restore the previous long polling timeout
                config:set(connection_timeout, PreviousTimeout),
                fixtures:teardown([Domain, BaseUrl, Testers])
        end
      , fun([Domain, BaseUrl, [Root|_], _]) ->
                [
                 {timeout, 40, ?_test(test_long_polling_with_from(BaseUrl, Root))},
                 {timeout, 40, ?_test(test_long_polling_with_other_from(BaseUrl, Root))},
                 {timeout, 40, ?_test(test_long_polling_with_parent(Domain, BaseUrl, Root))},
                 {timeout, 40, ?_test(test_long_polling_with_other_parent(BaseUrl, Root))},
                 {timeout, 40, ?_test(test_long_polling_with_search(BaseUrl, Root))},
                 {timeout, 40, ?_test(test_long_polling_with_other_search(BaseUrl, Root))}
                ]
        end}.

auth_params({Uid, Sid}) ->
    [{"uid", Uid},
     {"sid", Sid}].

post_event(BaseUrl, Meeting, AuthParams, Params) ->
    {struct, [{"result", Id}]} = tests_utils:post(BaseUrl, "/event/"++ Meeting, Params ++ auth_params(AuthParams)),
    Id.

test_push(BaseUrl, {RootUid, _RootSid} = AuthParams) ->
    Params = [{"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    Id = post_event(BaseUrl, "testmeeting", AuthParams, Params),
    ?assertMatch({struct, [{"result",
                            {struct, [{"type", "test_push_1"},
                                      {"domain", _},
                                      {"datetime", _},
                                      {"id", Id},
                                      {"location", "testmeeting"},
                                      {"from", RootUid},
                                      {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, auth_params(AuthParams))).

test_push_big(BaseUrl, {RootUid, _RootSid} = AuthParams) ->
    Text = string:copies("pushed_event", 5000),
    Params = [{"type", "test_push_1"},
              {"metadata[description]", Text}],
    Id = post_event(BaseUrl, "testmeeting", AuthParams, Params),
    ?assertMatch({struct, [{"result",
                            {struct, [{"type", "test_push_1"},
                                      {"domain", _},
                                      {"datetime", _},
                                      {"id", Id},
                                      {"location", "testmeeting"},
                                      {"from", RootUid},
                                      {"metadata", {struct, [{"description", Text}]}}]}}]},
                 tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, auth_params(AuthParams))).

test_push_internal_event(BaseUrl, AuthParams) ->
    Params = [{"type", "internal.roster.add"},
              {"metadata[description]", "pushed_event"}] ++ auth_params(AuthParams),
    {struct, [{"error", "unauthorized"}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_push_without_meeting(BaseUrl, {RootUid, _RootSid} = AuthParams) ->
    Params = [{"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    Id = post_event(BaseUrl, "", AuthParams, Params),
    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/all/" ++ Id, auth_params(AuthParams)).

test_push_with_parent(BaseUrl, {RootUid, _RootSid} = AuthParams) ->
    Params = [{"type", "test_push_1"},
              {"metadata[description]", "pushed_event"}],
    ParentId = post_event(BaseUrl, "testmeeting", AuthParams, Params),

    ParamsChild = [{"type", "test_push_1"},
                   {"parent", ParentId},
                   {"metadata[description]", "pushed_event"}],
    ChildId = post_event(BaseUrl,"testmeeting", AuthParams, ParamsChild),

    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", ChildId},
                         {"location", "testmeeting"},
                         {"from", RootUid},
                         {"parent", ParentId},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ ChildId, auth_params(AuthParams)).

test_push_to_me(BaseUrl, {RootUid, _RootSid} = AuthParams) ->
    Params = [{"type", "test_push_1"},
              {"to", RootUid},
              {"metadata[description]", "pushed_event"}],
    Id = post_event(BaseUrl, "testmeeting", AuthParams, Params),
    {struct, [{"result",
               {struct, [{"type", "test_push_1"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"location", "testmeeting"},
                         {"to", RootUid},
                         {"from", RootUid},
                         {"metadata", {struct, [{"description", "pushed_event"}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, auth_params(AuthParams)).

test_push_to_unauthorized(BaseUrl,
                          RootAuthParams,
                          ParticipantParams,
                          {UglyUid, _UglySid}) ->
    Params = auth_params(ParticipantParams) ++ [{"type", "test_push_1"},
                                                {"to", UglyUid},
                                                {"metadata[description]", "pushed_event"}],
    {struct, [{"result", _Id}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    ParamsRoot = auth_params(RootAuthParams) ++ [{"type", "test_push_to_other"}],
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

test_push_json(BaseUrl, AuthParams) ->
    RequestBody = {struct, [{"type", "test_json"},
                            {"metadata", {struct, [{"complex", {array, [10, {struct, [{"name", "plip"}]}]}}]}}]
                                ++ auth_params(AuthParams)},
    {ok, "201", _, Body} =
        tests_utils:post_raw(BaseUrl, "/event/testmeeting", [], "application/json", mochijson:encode(RequestBody)),
    {struct, [{"result", Id}]} = mochijson:decode(Body),
    {struct, [{"result",
               {struct, [{"type", "test_json"},
                         {"domain", _},
                         {"datetime", _},
                         {"id", Id},
                         {"location", "testmeeting"},
                         {"from", _},
                         {"metadata", {struct, [{"complex", {array, [10, {struct, [{"name", "plip"}]}]}}]}}]}}]} =
                   tests_utils:get(BaseUrl, "/event/testmeeting/" ++ Id, auth_params(AuthParams)).

test_push_missing_type(BaseUrl, AuthParams) ->
    Params = auth_params(AuthParams) ++ [{"metadata[description]", "pushed_event"}],
    {struct, [{"error", "missing_parameters"}, {"infos", _}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_push_not_found_meeting(BaseUrl, AuthParams) ->
    Params = auth_params(AuthParams) ++ [{"type", "test_push_1"},
                                         {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/unexistentmeeting", Params).

test_push_not_found_parent(BaseUrl, AuthParams) ->
    ParamsChild = auth_params(AuthParams) ++ [{"type", "test_push_1"},
                                              {"parent", "unexistent_id"},
                                              {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", ParamsChild).

test_push_not_found_to(BaseUrl, AuthParams) ->
    Params = auth_params(AuthParams) ++ [{"type", "test_push_1"},
                                         {"to", "unexistent_user"},
                                         {"metadata[description]", "pushed_event"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/event/testmeeting", Params).

test_get(BaseUrl, AuthParams) ->
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
                         }}]}, tests_utils:get(BaseUrl, "/event/testmeeting", auth_params(AuthParams))).

test_get_without_meeting(BaseUrl, AuthParams) ->
    Params = [{"type", "non_global_event"},
              {"metadata[description]", "plop2"}],
    post_event(BaseUrl, "testmeeting", AuthParams, Params),

    timer:sleep(1000),

    ParamsGet = auth_params(AuthParams) ++ [{"order", "desc"},
                                            {"count", "1"}],
    Result = tests_utils:get(BaseUrl, "/event/", ParamsGet),
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "non_global_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", _}
                                     , {"metadata", {struct, [{"description", "plop2"}]}}
                                    ]}]}}]},
                 Result).

test_get_with_keywords(BaseUrl, AuthParams) ->
    Params = [{"type", "search_event"},
              {"metadata[description]", "lonely event"}],
    post_event(BaseUrl, "testmeeting", AuthParams, Params),

    timer:sleep(1000),

    ParamsGet = auth_params(AuthParams) ++ [{"search", "lonely"},
                                            {"count", "1"}],
    ?assertMatch({struct, [{"result", {array,
                          [{struct, [{"type", "search_event"}
                                     , {"domain", _}
                                     , {"datetime", _}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", _}
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
    send_long_polling_event(BaseUrl, {RootUid, RootSid}, []).
send_long_polling_event(BaseUrl, {RootUid, RootSid}, ParamsEvent) ->
    timer:sleep(4000),
    Params = ParamsEvent ++[{"uid", RootUid},
                            {"sid", RootSid},
                            {"type", "long_polling_event"},
                            {"metadata[description]", "relax, don't do it"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params).

do_long_polling(BaseUrl, {RootUid, RootSid}, Now, Params) ->
    ParamsGet = Params ++ [{"uid", RootUid},
                           {"sid", RootSid},
                           {"start", integer_to_list(Now)},
                           {"mode", "longpolling"}],
    tests_utils:get(BaseUrl, "/live/testmeeting", ParamsGet).

assert_long_polling(BaseUrl, AuthParams = {RootUid, _RootSid}, Params) ->
    Now = utils:now(),
    Result = do_long_polling(BaseUrl, AuthParams, Now, Params),
    {struct, [{"result", {array,
                          [{struct, [{"type", "long_polling_event"}
                                     , {"domain", _}
                                     , {"datetime", _Datetime}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"metadata", {struct, [{"description", "relax, don't do it"}]}}
                                    ]}]}}]} = Result.

assert_long_polling_parent(BaseUrl, AuthParams = {RootUid, _RootSid}, Params, Parent) ->
    Now = utils:now(),
    Result = do_long_polling(BaseUrl, AuthParams, Now, Params),
    {struct, [{"result", {array,
                          [{struct, [{"type", "long_polling_event"}
                                     , {"domain", _}
                                     , {"datetime", _Datetime}
                                     , {"id", _}
                                     , {"location", "testmeeting"}
                                     , {"from", RootUid}
                                     , {"parent", Parent}
                                     , {"metadata", {struct, [{"description", "relax, don't do it"}]}}
                                    ]}]}}]} = Result.

assert_no_result_long_polling(BaseUrl, AuthParams, Params) ->
    Now = utils:now(),
    Result = do_long_polling(BaseUrl, AuthParams, Now, Params),
    {struct, [{"result", {array, []}}]} = Result.

test_long_polling(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_long_polling(BaseUrl, AuthParams, [{"type", "long_polling_event"}]).

test_long_polling_with_types(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_long_polling(BaseUrl, AuthParams, [{"type", "long_polling_event,long_polling_event2"}]).

test_long_polling_with_from(BaseUrl, AuthParams = {RootUid, _}) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_long_polling(BaseUrl, AuthParams, [{"from", RootUid}]).

test_long_polling_with_other_from(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_no_result_long_polling(BaseUrl, AuthParams, [{"from", "other_from"}]).

test_long_polling_with_parent(Domain, BaseUrl, AuthParams = {RootUid, _}) ->
    {ok, EventId} = uce_event:add(Domain,
                                  #uce_event{ id=none,
                                              type="test_long_polling_for_parent",
                                              location="testmeeting",
                                              from=RootUid}),
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams, [{"parent", EventId}]]),
    assert_long_polling_parent(BaseUrl, AuthParams, [{"parent", EventId}], EventId).

test_long_polling_with_other_parent(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_no_result_long_polling(BaseUrl, AuthParams, [{"parent", "other_parent"}]).

test_long_polling_with_search(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_long_polling(BaseUrl, AuthParams, [{"search", "relax"}]).

test_long_polling_with_other_search(BaseUrl, AuthParams) ->
    spawn(?MODULE, send_long_polling_event, [BaseUrl, AuthParams]),
    assert_no_result_long_polling(BaseUrl, AuthParams, [{"search", "plop"}]).
