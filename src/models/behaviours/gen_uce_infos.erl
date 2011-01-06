-module(gen_uce_infos).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get, 0},
     {update, 1}];

behaviour_info(_) ->
    undefined.
