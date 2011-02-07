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

-export([add/2, delete/2, update/5, update/2, list/1, get/2, exists/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(Domain, #uce_user{} = User) ->
    case ?MODULE:exists(Domain, User#uce_user.uid) of
	true ->
	    {error, conflict};
	false ->
	    ?DB_MODULE:add(Domain, User)
    end.

delete(Domain, Uid) ->
    case ?MODULE:get(Domain, Uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Domain, Uid)
    end.

update(Domain, Uid, Auth, Credential, Metadata) ->
    ?MODULE:update(Domain, #uce_user{uid=Uid,
                                     auth=Auth,
                                     credential=Credential,
                                     metadata=Metadata}).

update(Domain, #uce_user{} = User) ->
    case ?MODULE:get(Domain, User#uce_user.uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Domain, User)
    end.

list(Domain) ->
    ?DB_MODULE:list(Domain).

get(Domain, Uid) ->
    ?DB_MODULE:get(Domain, Uid).

exists(Domain, Uid) ->
    case ?MODULE:get(Domain, Uid) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.
