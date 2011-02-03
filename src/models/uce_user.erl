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

-export([add/1, delete/1, update/4, update/1, list/0, get/1, exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_user{} = User) ->
    case ?MODULE:exists(User#uce_user.uid) of
	true ->
	    {error, conflict};
	false ->
	    ?DB_MODULE:add(User)
    end.

delete(Uid) ->
    case ?MODULE:get(Uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Uid)
    end.

update(Uid, Auth, Credential, Metadata) ->
    ?MODULE:update(#uce_user{uid=Uid,
			     auth=Auth,
			     credential=Credential,
			     metadata=Metadata}).

update(#uce_user{} = User) ->
    case ?MODULE:get(User#uce_user.uid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(User)
    end.

list() ->
    ?DB_MODULE:list().

get(Uid) ->
    ?DB_MODULE:get(Uid).

exists(Uid) ->
    case ?MODULE:get(Uid) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.
