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

-export([add/2,
         delete/2,
         update/2,
         list/1,
         get/2]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_user,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, uce_user)}]).

add(_Domain, #uce_user{} = User) ->
    case mnesia:dirty_write(User) of 
        ok -> {ok, created};
        _=_Other -> {error, bad_parameters}
    end.

delete(_Domain, Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_user, Id})
                            end) of
        {atomic, ok} ->
            {ok, deleted};
        {aborted, _} ->
            {error, bad_parameters}
    end.

update(_Domain, #uce_user{} = User) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(User)
                            end) of
        {atomic, _} ->
            {ok, updated};
        {aborted, _} ->
            {error, bad_parameters}
    end.

list(Domain) ->
    Records = mnesia:dirty_match_object(#uce_user{id={'_', Domain},
                                            name='_',
                                            auth='_',
                                            credential='_',
                                            metadata='_',
                                            roles='_'}),
    {ok, Records}.

get(_Domain, Id) when is_list(Id) ->
    case mnesia:dirty_match_object({uce_user, '_', Id, '_', '_', '_', '_'}) of
        [] -> {error, not_found};
        [Record] -> {ok, Record};
        _=_Other -> {error, bad_parameters}
    end;
get(_Domain, {_, _} = Id) ->
    case mnesia:dirty_read(uce_user, Id) of
        [] -> {error, not_found};
        [Record] -> {ok, Record};
        _=_Other -> {error, bad_parameters}
    end.

drop() ->
    mnesia:clear_table(uce_user).
