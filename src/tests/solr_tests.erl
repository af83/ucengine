-module(solr_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    {ok, created} = uce_event_solr_search:add(#uce_event{id=utils:random(),
							 datetime=utils:now(),
							 location=["testmeeting"],
							 from="chuck_norris",
							 type="test_solr_event",
							 metadata=[{"text","This is a test event."}]}).

test_search() ->
    ok = case uce_event_solr_search:list(['_'], ["This is"], '_', '_', 0, infinity, '_') of
	     {error, Reason} ->
		 {error, Reason};
	     {ok, Results} when is_list(Results) ->
		 ok
	 end.

