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

-include("uce.hrl").
-include("mongodb.hrl").

%% gen_uce_infos api
-export([get/1, update/1]).

get(Domain) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", [{"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Record] ->
            {ok, from_collection(Record)};
        [] ->
            {ok, #uce_infos{domain=Domain, metadata=[]}};
        _ ->
            throw({error, bad_parameters})
    end.

update(#uce_infos{domain=Domain} = Infos) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_infos", [{"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [_] ->
            case catch emongo:update_sync(?MONGO_POOL, "uce_infos",
                                          [{"domain", Domain}],
                                          to_collection(Infos), false) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("~p~n", [Reason]),
                    throw({error, bad_parameters});
                _ ->
                    {ok, updated}
            end;
        [] ->
            case catch emongo:insert_sync(?MONGO_POOL, "uce_infos", to_collection(Infos)) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("~p~n", [Reason]),
                    throw({error, bad_parameters});
                _ ->
                    {ok, updated}
            end;
        _ ->
            throw({error, bad_parameters})
    end.

to_collection(#uce_infos{domain=Domain,
                         metadata=Metadata}) ->
    [{"domain", Domain},
     {"metadata", Metadata}].

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["domain", "metadata"]) of
        [Domain, Metadata] ->
            #uce_infos{domain=Domain, metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.
