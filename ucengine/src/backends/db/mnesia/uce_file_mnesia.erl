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

-include_lib("stdlib/include/qlc.hrl").
-include("uce.hrl").

-author('victor.goya@af83.com').

-behaviour(gen_uce_file).

-export([init/0, drop/0]).

-export([add/2, list/3, get/2, all/1, delete/2]).

init() ->
    case mnesia:create_table(uce_file,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_file)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_file}} -> ok
    end.

add(_Domain, #uce_file{} = File) ->
    case mnesia:dirty_write(File) of
        ok ->
            {ok, File#uce_file.id};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

list(_Domain, {_, _}=Location, Order) ->
    FunOrder = case Order of
                   asc ->
                       fun(A, B) ->
                               B#uce_file.datetime > A#uce_file.datetime
                       end;
                   desc ->
                       fun(A, B) ->
                               B#uce_file.datetime < A#uce_file.datetime
                       end
               end,
    Transaction = fun() ->
                          Query = qlc:q([File || File <- mnesia:table(uce_file), File#uce_file.location == Location]),
                          qlc:eval(qlc:sort(Query, [{order, FunOrder}]))
          end,
    case mnesia:transaction(Transaction) of
        {atomic, Files} ->
            {ok, Files};
        {aborded, Error} ->
                ?ERROR_MSG("ERROR ~p~n", [Error]),
                throw({error, Error})
    end.

all(Domain) ->
        case mnesia:dirty_match_object(#uce_file{id={'_', Domain},
                                                 name='_',
                                                 location='_',
                                                 uri='_',
                                                 datetime='_',
                                                 mime='_',
                                                 metadata='_'}) of
            Files when is_list(Files) ->
                {ok, Files};
            {aborted, _} ->
                throw({error, bad_parameters})
    end.

get(_Domain, {_, _}=Id) ->
    case mnesia:dirty_read(uce_file, Id) of
        [File] ->
            {ok, File};
        [] ->
            throw({error, not_found});
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

delete(_Domain, {_, _}=Id) ->
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
