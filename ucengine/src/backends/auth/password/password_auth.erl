%%
%%  U.C.Engine - Unified Collaboration Engine
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

-include("uce.hrl").

-export([assert/5, check/5]).

assert(Domain, OwnerUid, OwnerSid, User, Credential) ->
    case check(Domain, OwnerUid, OwnerSid, User, Credential) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, bad_credentials})
    end.

check(_Domain, _OwnerUid, _OwnerSid, User, Credential) ->
    case User#uce_user.credential of
        Credential ->
            {ok, true};
        _ ->
            {ok, false}
    end.
