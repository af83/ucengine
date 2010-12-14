-module(time_tests).

-include_lib("eunit/include/eunit.hrl").

time_test_() ->
    { setup,
      fun fixtures:setup/0,
      fun fixtures:teardown/1,
      fun(_) ->
	      [?_test(test_get())]
      end}.

test_get() ->
    {struct, [{"result", Time}]} = tests_utils:get("/time", []),
    Diff = Time - utils:now(),
    if
	Diff > 1000 ->
	    throw({error, too_much_delay, Diff});
	true ->
	    nothing
    end.
