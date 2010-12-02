-module(gen_uce_presence).

-author('victor.goya@af83.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{add, 1},
     {list, 1},
     {get, 1},
     {delete, 1},
     {update, 1}];

behaviour_info(_) ->
    undefined.
