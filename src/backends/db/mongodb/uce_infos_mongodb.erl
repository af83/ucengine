%%
%%  U.C.Engine - Unified Colloboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(uce_infos_mongodb).

-behaviour(gen_uce_infos).

-include("mongodb.hrl").

%% gen_uce_infos api
-export([get/1, update/2]).

get(_Domain) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", []) of
        [Infos] ->
            {"metadata", Metadata} = mongodb_helpers:get_item_from_collection("metadata", Infos),
            {ok, Metadata};
        [] ->
            {ok, []}
    end.

update(_Domain, Metadata) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", [{"id", "default"}]) of
        [_Infos] ->
            emongo:update_sync(?MONGO_POOL, "uce_infos", [{"id", "default"}], [{"metadata", Metadata}], false);
        [] ->
            case catch emongo:insert_sync(?MONGO_POOL, "uce_infos", [{"id", "default"}, {"metadata", Metadata}]) of
                {'EXIT', _} ->
                    {error, bad_parameters};
                _ ->
                    {ok, updated}
            end
    end.
