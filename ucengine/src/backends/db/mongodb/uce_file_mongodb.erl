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

-export([add/2,
         list/3,
         all/1,
         get/2,
         delete/2]).

-include("uce.hrl").
-include("mongodb.hrl").

%%--------------------------------------------------------------------
%% @spec (#uce_file{}) -> {ok, Id}
%% @doc Insert given record #uce_file{} in uce_file mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_file{} = File) ->
    emongo:insert_sync(Domain, "uce_file", to_collection(File)),
    {ok, File#uce_file.id}.

%%--------------------------------------------------------------------
%% @spec (Domain, Location::list) -> {ok, [#uce_file{}, #uce_file{}, ..] = Files::list} | {error, bad_parameters}
%% @doc List all record #uce_file for the given pair location(meeting) and domain
%% @end
%%--------------------------------------------------------------------
list(Domain, Location, Order) ->
    Files = emongo:find_all(Domain, "uce_file", [{"location", Location},
                                                 {"domain", Domain}],
                            [{orderby, [{"datetime", Order}]}]),
    {ok, [from_collection(File) || File <- Files]}.

%%--------------------------------------------------------------------
%% @spec ({Location::list, Domain::list}) -> {ok, [#uce_file{}, #uce_file{}, ..] = Files::list} | {error, bad_parameters}
%% @doc List all record #uce_file for the given domain
%% @end
%%--------------------------------------------------------------------
all(Domain) ->
    Files = emongo:find_all(Domain, "uce_file", [{"domain", Domain}]),
    {ok, [from_collection(File) || File <- Files]}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, FileId::list) -> {ok, #uce_file{}} | {error, bad_parameters} | {error, not_found}
%% @doc Get #uce_file record for the given id
%% @end
%%--------------------------------------------------------------------
get(Domain, FileId) ->
    case emongo:find_one(Domain, "uce_file", [{"id", FileId}]) of
        [File] ->
            {ok, from_collection(File)};
        [] ->
            throw({error, not_found})
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, FileId::list) -> {ok, deleted}
%% @doc Delete #uce_file record for the given id
%% @end
%%--------------------------------------------------------------------
delete(Domain, FileId) ->
    mongodb_helpers:ok(emongo:delete_sync(Domain, "uce_file", [{"id", FileId}])),
    {ok, deleted}.

%%--------------------------------------------------------------------
%% @spec ([{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list) -> #uce_file{} | {error, bad_parameters}
%% @doc Convert collection returned by mongodb to valid record #uce_file{}
%% @end
%%--------------------------------------------------------------------
from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["id", "domain", "location", "name", "datetime", "mime", "uri", "metadata"]) of
        [Id, Domain, Location, Name, Datetime, Mime, Uri, Metadata] ->
            #uce_file{id={Id, Domain},
                      name=Name,
                      location={Location, Domain},
                      uri=Uri,
                      datetime=Datetime,
                      mime=Mime,
                      metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.

%%--------------------------------------------------------------------
%% @spec (#uce_file{}) -> [{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list
%% @doc Convert #uce_file{} record to valid collection
%% @end
%%--------------------------------------------------------------------
to_collection(#uce_file{id={Id, Domain},
                        name=Name,
                        location={Location, _},
                        uri=Uri,
                        datetime=Datetime,
                        mime=Mime,
                        metadata=Metadata}) ->
    [{"id", Id},
     {"domain", Domain},
     {"location", Location},
     {"name", Name},
     {"datetime", Datetime},
     {"mime", Mime},
     {"uri", Uri},
     {"metadata", Metadata}].
