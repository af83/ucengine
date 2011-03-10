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

-export([add/2, delete/2, update/2, list/1, get/2, exists/2]).

-include("uce.hrl").

add(Domain, #uce_user{id=Id} = User) ->
    case exists(Domain, Id) of
        true ->
            throw({error, conflict});
        false ->
            apply(db:get(?MODULE, Domain), add, [User])
    end.

delete(Domain, Id) ->
    case exists(Domain, Id) of
        true ->
            apply(db:get(?MODULE, Domain), delete, [Id]);
        false ->
            throw({error, not_found})
    end.

update(Domain, #uce_user{id=Id} = User) ->
    case ?MODULE:exists(Domain, Id) of
        true ->
            apply(db:get(?MODULE, Domain), update, [User]);
        false ->
            throw({error, not_found})
    end.

list(Domain) ->
    apply(db:get(?MODULE, Domain), list, [Domain]).

get(Domain, User) ->
    apply(db:get(?MODULE, Domain), get, [User]).

exists(Domain, Id) ->
    case Id of
        {"", _} -> % all
            true;
        _ ->
            case catch ?MODULE:get(Domain, Id) of
                {error, not_found} ->
                    false;
                {error, Reason} ->
                    throw({error, Reason});
                _ ->
                    true
            end
    end.
