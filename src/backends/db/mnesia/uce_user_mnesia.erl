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
-module(uce_user_mnesia).

-author('victor.goya@af83.com').

-export([init/0, drop/0]).

-export([add/1,
         delete/1,
         update/1,
         list/1,
         get/1]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_user,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, uce_user)}]).

add(#uce_user{} = User) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(User)
                            end) of
        {atomic, _} ->
            {ok, created};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

delete(Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_user, Id})
                            end) of
        {atomic, ok} ->
            {ok, deleted};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

update(#uce_user{} = User) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(User)
                            end) of
        {atomic, _} ->
            {ok, updated};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

list(Domain) ->
    case mnesia:transaction(fun() ->
                                    mnesia:match_object(#uce_user{id={'_', Domain},
                                                                  auth='_',
                                                                  credential='_',
                                                                  metadata='_'})
                            end) of
        {atomic, Records} ->
            {ok, Records};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

get(Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:read(uce_user, Id)
                            end) of
        {atomic, [Record]} ->
            {ok, Record};
        {atomic, _} ->
            throw({error, not_found});
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

drop() ->
    mnesia:clear_table(uce_user).
