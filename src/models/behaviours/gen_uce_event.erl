-module(gen_uce_event).

-author('victor.goya@af83.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{add, 1},
     {get, 1},
     {list, 6}];

behaviour_info(_) ->
    undefined.
