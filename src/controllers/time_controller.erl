-module(time_controller).

-export([init/0, get/3]).

-include("uce.hrl").

init() ->
    [#uce_route{method='GET',
		regexp="/time",
		callbacks=[{?MODULE, get, [], [], []}]}].

get(_, _, _) ->
    json_helpers:json(utils:now()).
