-module(uce_infos_mongodb).

-behaviour(gen_uce_infos).

-include("mongodb.hrl").

%% gen_uce_infos api
-export([get/0, update/1]).

get() ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", []) of
        [Infos] ->
            {"metadata", Metadata} = mongodb_helpers:get_item_from_collection("metadata", Infos),
            {ok, Metadata};
        [] ->
            {ok, []}
    end.

update(Metadata) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", [{"id", "default"}]) of
        [Infos] ->
            emongo:update_sync(?MONGO_POOL, "uce_infos", [{"id", "default"}], [{"metadata", Metadata}], false);
        [] ->
            case catch emongo:insert_sync(?MONGO_POOL, "uce_infos", [{"id", "default"}, {"metadata", Metadata}]) of
                {'EXIT', _} ->
                    {error, bad_parameters};
                _ ->
                    {ok, updated}
            end
    end.
