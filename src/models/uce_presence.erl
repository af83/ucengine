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
-module(uce_presence).

-author('tbomandouki@af83.com').

-export([add/1, get/1, delete/1, update/1, exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_presence{sid=[]}=Presence) ->
    add(Presence#uce_presence{sid=utils:random()});
add(#uce_presence{last_activity=undefined}=Presence) ->
    add(Presence#uce_presence{last_activity=utils:now()});
add(#uce_presence{}=Presence) ->
    ?DB_MODULE:add(Presence).

get(Sid) ->
    ?DB_MODULE:get(Sid).

delete(Sid) ->
    case ?MODULE:exists(Sid) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:delete(Sid)
    end.

update(#uce_presence{}=Presence) ->
    case ?MODULE:get(Presence#uce_presence.sid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Presence)
    end.

exists(Sid) ->
    case ?MODULE:get(Sid) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.
