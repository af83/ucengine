-module(solr_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_EVENT, {uce_event, "test_solr_event",
              1287652875728,
              ["af83","demo"],
              "root","test_solr_event",
              [{"text","This is test event, indexing by solr to test search"}]}
	   ).

-export([test_add/0, test_search/0]).

solr_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
	, fun(_) ->
        [ ?_test(test_add())
        , ?_test(test_search())
        ]
      end
    }.

test_add() ->
	R = case utils:get(utils:get(config:get(modules), solr), [host, token, blacklist]) of
		[_Host, _Token, _Blacklist] = Params->  Add = solr:add(?TEST_EVENT, Params),
											 io:format("Result test_add : ~p", [Add]),
											 Add;
		_ -> {error, "solr not configured"}
	end,
	ok = R.

test_search() ->
	R = case solr:search("af83", "demo", none, ["test_solr_event"], none, none, ["solr", "index"]) of
		{error, _Error} = TupleErr -> TupleErr;
		_ = Results -> ok
	end,
	ok = R.

