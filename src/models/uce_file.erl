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
-module(uce_file).

-author('victor.goya@af83.com').

-export([add/2, list/2, get/2, delete/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(Domain, #uce_file{location=Location, name=Name} = File) ->
    case location_helpers:exists(Domain, Location) of
	false ->
	    {error, not_found};
	true ->
	    {Id, Mime} =
		case re:run(Name, "([^/]+)\\.([^/]+)$ ?", [{capture, all, list}]) of
		    {match, [_, BareName, Extension]} ->
			{BareName ++ "_" ++ utils:random() ++ "." ++ Extension,
			 yaws_api:mime_type(Name)};
		    _ ->
			{Name ++ "_" ++ utils:random(), "text/plain"}
		end,
	    case ?DB_MODULE:add(Domain, File#uce_file{id=Id, mime=Mime}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    {ok, Id}
	    end
    end.

list(Domain, Location) ->
    case location_helpers:exists(Domain, Location) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:list(Domain, Location)
    end.

get(Domain, Id) ->
    ?DB_MODULE:get(Domain, Id).

delete(Domain, Id) ->
    case ?MODULE:get(Domain, Id) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Domain, Id)
    end.
