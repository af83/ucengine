-module('db').

-export([get/2]).

get(Module, Domain) ->
    case config:get(Domain, db) of
        undefined -> throw({error, no_database});
        _ = Value -> list_to_atom(lists:concat([Module, "_", Value]))
    end.
