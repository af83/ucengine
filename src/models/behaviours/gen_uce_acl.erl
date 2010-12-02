-module(gen_uce_acl).

-author('victor.goya@af83.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{add, 1},
     {delete, 5}];

behaviour_info(_) ->
    undefined.
