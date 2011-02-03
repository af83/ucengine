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
-module(uce_infos_mnesia).

-behaviour(gen_uce_infos).
%% gen_uce_infos api
-export([get/0, update/1]).

-export([init/0, drop/0]).

-record(uce_infos, {
          id = none,
          metadata = []}).

% ambiguous call of overridden pre R14 auto-imported BIF get/1
% - use erlang:get/1 or "-compile({no_auto_import,[get/1]})." to resolve name clash
-compile({no_auto_import,[get/1]}).


init() ->
    catch mnesia:create_table(uce_infos,
                              [{disc_copies, [node()]},
                               {type, set},
                               {attributes, record_info(fields, uce_infos)}]).

get() ->
    case get(default) of
        {atomic, [#uce_infos{metadata = Metadata}]} ->
            {ok, Metadata};
        {atomic, []} ->
            {ok, []};
        {aborted, Reason} ->
            {error, Reason}
    end.

get(Id) ->
    mnesia:transaction(fun() ->
                               mnesia:read({uce_infos, Id})
                       end).

update(Metadata) ->
    case get(default) of
        {atomic, [Infos]} ->
            case mnesia:transaction(fun() ->
                                            mnesia:write(Infos#uce_infos{metadata=Metadata})
                                    end) of
                {atomic, _} ->
                    {ok, updated};
                {aborted, Reason} ->
                    {error, Reason}
            end;
        _ ->
            case mnesia:transaction(fun() ->
                                            mnesia:write(#uce_infos{id=default, metadata = Metadata})
                                    end) of
                {atomic, _} ->
                    {ok, updated};
                {aborted, Reason} ->
                    {error, Reason}
            end
    end.

drop() ->
    mnesia:clear_table(uce_infos).
