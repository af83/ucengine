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
-module(uce_user).

-author('tbomandouki@af83.com').

-export([add/1, delete/1, update/1, list/1, get/1, exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_user{id=Id} = User) ->
    case exists(Id) of
        true ->
            throw({error, conflict});
        false ->
            ?DB_MODULE:add(User)
    end.

delete(Id) ->
    case exists(Id) of
        true ->
            ?DB_MODULE:delete(Id);
        false ->
            throw({error, not_found})
    end.

update(#uce_user{id=Id} = User) ->
    case ?MODULE:exists(Id) of
        true ->
            ?DB_MODULE:update(User);
        false ->
            throw({error, not_found})
    end.

list(Domain) ->
    ?DB_MODULE:list(Domain).

get(Id) ->
    ?DB_MODULE:get(Id).

exists(Id) ->
    case Id of
        {"", _} -> % all
            true;
        _ ->
            case catch ?MODULE:get(Id) of
                {error, not_found} ->
                    false;
                {error, Reason} ->
                    throw({error, Reason});
                _ ->
                    true
            end
    end.
