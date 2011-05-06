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
-module(uce_role_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_role).

-export([init/0, drop/0]).

-export([add/2,
         delete/2,
         update/2,
         get/2]).

-include("uce.hrl").

init() ->
    case mnesia:create_table(uce_role,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_role)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_role}} -> ok
    end.

add(_Domain, #uce_role{}=Role) ->
    case mnesia:dirty_write(Role) of
        ok ->
            {ok, created};
        {aborted, Reason} ->
            {error, Reason}
    end.

update(_Domain, #uce_role{}=Role) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(Role)
                            end) of
        {atomic, _} ->
            {ok, updated};
        {aborted, Reason} ->
            {error, Reason}
    end.

delete(_Domain, Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_role, Id})
                            end) of
        {atomic, _} ->
            {ok, deleted};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

get(_Domain, Id) ->
    case mnesia:dirty_read(uce_role, Id) of
        [Record] ->
            {ok, Record};
        [] ->
            throw({error, not_found});
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

drop() ->
    mnesia:clear_table(uce_role).
