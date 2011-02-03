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
-module(uce_file_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_file).

-export([add/1,
	 list/1,
	 get/1,
	 delete/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_file{} = File) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_file", to_collection(File)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

list(Location) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_file", [{"location", Location}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Files ->
	    {ok, [from_collection(File) || File <- Files]}
    end.

get(Id) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_file", [{"id", Id}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[File] ->
	    {ok, from_collection(File)};
	_ ->
	    {error, not_found}
    end.

delete(Id) ->
    case catch emongo:delete(?MONGO_POOL, "uce_file", [{"id", Id}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, deleted}
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["id", "location", "name", "uri", "metadata"]) of
	[Id, Location, Name, Uri, Metadata] ->	
	    #uce_file{id=Id,
			name=Name,
			location=Location,
			uri=Uri,
			metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_file{id=Id, name=Name, location=Location, uri=Uri, metadata=Metadata}) ->
    [{"id", Id},
     {"location", Location},
     {"name", Name},
     {"uri", Uri},
     {"metadata", Metadata}].
