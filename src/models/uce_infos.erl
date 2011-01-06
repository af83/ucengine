-module(uce_infos).

-include("uce_models.hrl").

-export([get/0, update/1]).

get() ->
    ?DB_MODULE:get().

update(Metadata) ->
    ?DB_MODULE:update(Metadata).
