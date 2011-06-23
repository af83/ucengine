-module('db').

-export([get/2]).
-include("uce.hrl").

get(Module, Domain) ->
    ?COUNTER(lists:flatten(io_lib:format("db:get:~s", [Module]))),
    case config:get(Domain, db) of
        undefined -> throw({error, no_database});
        _ = Value -> list_to_atom(lists:concat([Module, "_", Value]))
    end.
