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
-module(search_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_events(Domain) ->
    uce_event:add(Domain, #uce_event{ domain=Domain,
                              type="test_event_1",
                              location={"testmeeting", Domain},
                              from={"participant.user@af83.com", Domain}}),
    timer:sleep(10),
    uce_event:add(Domain, #uce_event{ domain=Domain,
                              type="test_event_2",
                              location={"testmeeting", Domain},
                              from={"user_2", Domain}}),
    timer:sleep(10),
    uce_event:add(Domain, #uce_event{ domain=Domain,
                              type="test_event_3",
                              location={"testmeeting", Domain},
                              from={"user_3", Domain},
                              metadata=[{"description", "test"}]}),

    timer:sleep(2000),

    {ok, [Event1]} = uce_event:list(Domain,
                                    {"", Domain},
                                    [],
                                    {"", Domain},
                                    ["test_event_1"],
                                    {"", Domain},
                                    0, infinity, "", 0, 1, asc),

    {ok, [Event2]} = uce_event:list(Domain,
                                    {"", Domain},
                                    [],
                                    {"", Domain},
                                    ["test_event_2"],
                                    {"", Domain},
                                    0, infinity, "", 0, 1, asc),

    {ok, [Event3]} = uce_event:list(Domain,
                                    {"", Domain},
                                    [],
                                    {"", Domain},
                                    ["test_event_3"],
                                    {"", Domain},
                                    0, infinity, "", 0, 1, asc),

    [Event1, Event2, Event3].

search_test_() ->
    { setup
      , fun() ->
                [Domain, BaseUrl, Testers] = fixtures:setup(),
                Events = setup_events(Domain),
                [Domain, BaseUrl, Testers, Events]
        end
      , fun([Domain, BaseUrl, Testers, _Events]) ->
                fixtures:teardown([Domain, BaseUrl, Testers])
        end
      , fun([_, BaseUrl, Testers, Events]) ->
                [?_test(test_search(BaseUrl, Testers, Events)),
                 ?_test(test_search_first(BaseUrl, Testers, Events)),
                 ?_test(test_search_second(BaseUrl, Testers, Events)),
                 ?_test(test_search_second_page(BaseUrl, Testers, Events)),
                 ?_test(test_search_overflow(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_keywords(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_keywords_without_meeting(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_keywords_with_from(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_keywords_and_timestart_and_timeend(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_type(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_types(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_type_and_timestart(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_type_and_timestart_and_timeend(BaseUrl, Testers, Events)),
                 ?_test(test_search_with_type_and_timeend(BaseUrl, Testers, Events))
                ]
        end}.

-define(MATCH_SEARCH_RESULTS(TotalResults, StartIndex, ItemsPerPage, SearchTerms, StartPage, Entries, Results),
        ?assertMatch({struct, [{"result", {struct,
                                           [{"link", _},
                                            {"totalResults", TotalResults},
                                            {"startIndex", StartIndex},
                                            {"itemsPerPage", ItemsPerPage},
                                            {"Query", {struct, [{"role", "request"},
                                                                {"searchTerms", SearchTerms},
                                                                {"startPage", StartPage}]}},
                                            {"entries", Entries}]}}]},
                     Results)).

test_search(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"count", "3"}],

    ?MATCH_SEARCH_RESULTS(3, 0, 3, "", 1, {array, [{struct, [{"type", "test_event_1"}
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
                                                            ]}]
                                          },
                          tests_utils:get(BaseUrl, "/search/event", Params)).

test_search_first(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"count", "1"}],

    ?MATCH_SEARCH_RESULTS(1, 0, 1, "", 1, {array, [{struct, [{"type", "test_event_1"}
                                                             , {"domain", _}
                                                             , {"datetime", _}
                                                             , {"id", _}
                                                             , {"location", "testmeeting"}
                                                             , {"from", "participant.user@af83.com"}
                                                             , {"metadata", {struct, []}}
                                                            ]}]},
                          tests_utils:get(BaseUrl, "/search/event", Params)).

test_search_second(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"count", "1"},
              {"startIndex", "1"}],

    ?MATCH_SEARCH_RESULTS(1, 1, 1, "", 1, {array, [{struct, [{"type", "test_event_2"}
                                                             , {"domain", _}
                                                             , {"datetime", _}
                                                             , {"id", _}
                                                             , {"location", "testmeeting"}
                                                             , {"from", "user_2"}
                                                             , {"metadata", {struct, []}}
                                                            ]}]},
                          tests_utils:get(BaseUrl, "/search/event", Params)).

test_search_second_page(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"count", "1"},
              {"startPage", "2"}],

    ?MATCH_SEARCH_RESULTS(1, 0, 1, "", 2, {array, [{struct, [{"type", "test_event_2"}
                                                             , {"domain", _}
                                                             , {"datetime", _}
                                                             , {"id", _}
                                                             , {"location", "testmeeting"}
                                                             , {"from", "user_2"}
                                                             , {"metadata", {struct, []}}
                                                            ]}]},
                          tests_utils:get(BaseUrl, "/search/event", Params)).

test_search_overflow(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"count", "2"},
              {"startPage", "2"},
              {"startIndex", "1"}],

    ?MATCH_SEARCH_RESULTS(0, 1, 2, "", 2, {array, []},
                          tests_utils:get(BaseUrl, "/search/event", Params)).

test_search_with_keywords(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    timer:sleep(2000),

    SearchTerms = lists:concat([" type:search_event",
                                " location:testmeeting",
                                " lonely event"]),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"count", "1"},
                 {"searchTerms", SearchTerms}],

    ?MATCH_SEARCH_RESULTS(1, 0, 1, SearchTerms, 1, {array, [{struct, [{"type", "search_event"}
                                                                       , {"domain", _}
                                                                       , {"datetime", _}
                                                                       , {"id", _}
                                                                       , {"location", "testmeeting"}
                                                                       , {"from", RootUid}
                                                                       , {"metadata", {struct, [{"description", "lonely event"}]}}
                                                                      ]}]}, tests_utils:get(BaseUrl, "/search/event", ParamsGet)).

test_search_with_keywords_without_meeting(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely hungry event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    timer:sleep(2000),

    SearchTerms = lists:concat([" type:search_event",
                                " hungry"]),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"count", "1"},
                 {"searchTerms", SearchTerms}],

    ?MATCH_SEARCH_RESULTS(1, 0, 1, SearchTerms, 1, {array,
                                                   [{struct, [{"type", "search_event"}
                                                              , {"domain", _}
                                                              , {"datetime", _}
                                                              , {"id", _}
                                                              , {"location", "testmeeting"}
                                                              , {"from", RootUid}
                                                              , {"metadata", {struct, [{"description", "lonely hungry event"}]}}
                                                             ]}]},
                         tests_utils:get(BaseUrl, "/search/event", ParamsGet)).

test_search_with_keywords_with_from(BaseUrl, [{RootUid, RootSid}, _], _) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"type", "search_event"},
              {"metadata[description]", "lonely event"}],
    {struct, [{"result", _}]} = tests_utils:post(BaseUrl, "/event/testmeeting", Params),

    timer:sleep(2000),

    SearchTerms = lists:concat([" from:", RootUid,
                                " lonely"]),

    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"count", "1"},
                 {"searchTerms", SearchTerms}],

    ?MATCH_SEARCH_RESULTS(1, 0, 1, SearchTerms, 1, {array,
                                                    [{struct, [ {"type", "search_event"}
                                                                , {"domain", _}
                                                                , {"datetime", _}
                                                                , {"id", _}
                                                                , {"location", "testmeeting"}
                                                                , {"from", RootUid}
                                                                , {"metadata", {struct, [{"description", "lonely event"}]}}
                                                              ]}]},
                          tests_utils:get(BaseUrl, "/search/event", ParamsGet)).

test_search_with_keywords_and_timestart_and_timeend(BaseUrl,
                                                    [{RootUid, RootSid}, _],
                                                    [_, _, #uce_event{datetime = Datetime}]) ->
    SearchTerms = lists:concat([" start:", Datetime,
                                " end:", Datetime + 1,
                                " test"]),

    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"searchTerms", SearchTerms}],
    ?MATCH_SEARCH_RESULTS(1, 0, 10, SearchTerms, 1, {array,
                                                     [ {struct, [{"type", "test_event_3"}
                                                                 , {"domain", _}
                                                                 , {"datetime", Datetime}
                                                                 , {"id", _}
                                                                 , {"location", "testmeeting"}
                                                                 , {"from", "user_3"}
                                                                 , {"metadata", {struct, [{"description", "test"}]}}
                                                                ]}|_]
                                                    },
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)),

    SearchTermsNothing = lists:concat([" start:", Datetime - 2,
                                       " end:", Datetime - 1,
                                       " test"]),

    ParamsGetNothing = [{"uid", RootUid},
                        {"sid", RootSid},
                        {"searchTerms", SearchTermsNothing}],

    ?MATCH_SEARCH_RESULTS(0, 0, 10, SearchTermsNothing, 1, {array, []},
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetNothing)).

test_search_with_type(BaseUrl, [{RootUid, RootSid}, _], [_, _, #uce_event{datetime=Datetime}]) ->
     SearchTerms = lists:concat([" type:test_event_3"]),

     ParamsGetStart = [{"uid", RootUid},
                       {"sid", RootSid},
                       {"searchTerms", SearchTerms}],

     ?MATCH_SEARCH_RESULTS(_, 0, 10, SearchTerms, 1, {array,
                                                      [ {struct, [{"type", "test_event_3"}
                                                                  , {"domain", _}
                                                                  , {"datetime", Datetime}
                                                                  , {"id", _}
                                                                  , {"location", "testmeeting"}
                                                                  , {"from", "user_3"}
                                                                  , {"metadata", {struct, [{"description", "test"}]}}
                                                                 ]}|_]},
                           tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)).

test_search_with_types(BaseUrl, [{RootUid, RootSid}, _], [_, _, #uce_event{datetime=Datetime}]) ->
    SearchTerms = lists:concat([" type:test_event_3,test_event_1"]),

    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"searchTerms", SearchTerms}],

    ?MATCH_SEARCH_RESULTS(_, 0, 10, SearchTerms, 1, {array,
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
                                                                 , {"datetime", Datetime}
                                                                 , {"id", _}
                                                                 , {"location", "testmeeting"}
                                                                 , {"from", "user_3"}
                                                                 , {"metadata", {struct, [{"description", "test"}]}}
                                                                ]}|_]
                                                    },
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)).

test_search_with_type_and_timestart(BaseUrl, [{RootUid, RootSid}, _], [_, _, #uce_event{datetime=Datetime}]) ->
    SearchTerms = lists:concat([" start:", Datetime,
                                " type:test_event_3",
                                " test"]),

    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"searchTerms", SearchTerms}],

    ?MATCH_SEARCH_RESULTS(1, 0, 10, SearchTerms, 1, {array,
                                                     [ {struct, [{"type", "test_event_3"}
                                                                 , {"domain", _}
                                                                 , {"datetime", Datetime}
                                                                 , {"id", _}
                                                                 , {"location", "testmeeting"}
                                                                 , {"from", "user_3"}
                                                                 , {"metadata", {struct, [{"description", "test"}]}}
                                                                ]}|_]
                                                    },
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)).

test_search_with_type_and_timestart_and_timeend(BaseUrl, [{RootUid, RootSid}, _], [_, _, #uce_event{datetime=Datetime}]) ->

    SearchTerms = lists:concat([" start:", Datetime,
                                " end:", Datetime + 1,
                                " type:test_event_3",
                                " test"]),
    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"searchTerms", SearchTerms}],

        ?MATCH_SEARCH_RESULTS(1, 0, 10, SearchTerms, 1, {array,
                                                     [ {struct, [{"type", "test_event_3"}
                                                                 , {"domain", _}
                                                                 , {"datetime", Datetime}
                                                                 , {"id", _}
                                                                 , {"location", "testmeeting"}
                                                                 , {"from", "user_3"}
                                                                 , {"metadata", {struct, [{"description", "test"}]}}
                                                                ]}|_]
                                                    },
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)).

test_search_with_type_and_timeend(BaseUrl, [{RootUid, RootSid}, _], [_,
                                                                     #uce_event{datetime=Datetime1},
                                                                     #uce_event{datetime=Datetime2}]) ->
    SearchTerms = lists:concat([" end:", Datetime2 - 1,
                                " type:test_event_2"]),

    ParamsGetStart = [{"uid", RootUid},
                      {"sid", RootSid},
                      {"searchTerms", SearchTerms}],
    ?MATCH_SEARCH_RESULTS(1, 0, 10, SearchTerms, 1, {array,
                                                     [ {struct, [{"type", "test_event_2"}
                                                                 , {"domain", _}
                                                                 , {"datetime", Datetime1}
                                                                 , {"id", _}
                                                                 , {"location", "testmeeting"}
                                                                 , {"from", "user_2"}
                                                                 , {"metadata", {struct, []}}
                                                                ]}]
                                                    },
                          tests_utils:get(BaseUrl, "/search/event", ParamsGetStart)).
