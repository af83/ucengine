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
-module(uce_file_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_file).

-export([init/0, drop/0]).

-export([add/1, list/1, get/1, all/1, delete/1]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_file,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, uce_file)}]).

add(#uce_file{} = File) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(File)
                            end) of
        {atomic, _} ->
            {ok, File#uce_file.id};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

list(Location) ->
    case mnesia:transaction(fun() ->
                                    mnesia:match_object(#uce_file{id='_',
                                                                  domain='_',
                                                                  name='_',
                                                                  location=Location,
                                                                  uri='_',
                                                                  metadata='_'})
                            end) of
        {atomic, Files} ->
            {ok, Files};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

all(Domain) ->
        case mnesia:transaction(fun() ->
                                    mnesia:match_object(#uce_file{id='_',
                                                                  domain=Domain,
                                                                  name='_',
                                                                  location='_',
                                                                  uri='_',
                                                                  metadata='_'})
                                end) of
            {atomic, Files} ->
                {ok, Files};
            {aborted, _} ->
                throw({error, bad_parameters})
    end.

get(Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:read(uce_file, Id)
                            end) of
        {atomic, [File]} ->
            {ok, File};
        {atomic, []} ->
            throw({error, not_found});
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

delete(Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_file, Id})
                            end) of
        {atomic, ok} ->
            {ok, deleted};
        {aborted, _} ->
            throw({error, bad_paramaters})
    end.

drop() ->
    mnesia:clear_table(uce_file).
