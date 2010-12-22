-module(gen_uce_meeting).

-author('victor.goya@af83.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{add, 1},
     {get, 1},
     {list, 0},
     {delete, 1},
     {update, 1}];

behaviour_info(_) ->
    undefined.
