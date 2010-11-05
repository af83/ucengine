-module(tests).

-include("uce.hrl").

-export([run/0, start/0]).

start() ->
    case run() of
	error ->
	    init:stop(2);
	ok ->
	    init:stop(0)
    end.

run() ->
    Modules = [event_tests,
	       acl_tests,
	       meeting_tests,
	       org_tests,
	       presence_tests,
	       user_tests,
	       time_tests,
	       file_tests,
	       solr_tests],
    Failed = lists:filter(fun(Module) ->
				  io:format("> ~p~n", [Module]),
				  case Module:test() of
				      ok ->
					  false;
				      error ->
					  true
				  end
			  end, Modules),
    if
	length(Failed) > 0 ->	    
	    error;
	true ->
	    ok
    end.
