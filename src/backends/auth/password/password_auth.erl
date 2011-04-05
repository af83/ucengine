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
-module(password_auth).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([assert/2, check/2]).

assert(User, Credential) ->
    case ?MODULE:check(User, Credential) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, bad_credentials})
    end.

check(User, Credential) ->
    case User#uce_user.credential of
        Credential ->
            {ok, true};
        _ ->
            {ok, false}
    end.
