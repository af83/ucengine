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
-module(tests).

-include("uce.hrl").

-export([run/0, start/0]).

start() ->
    case run() of
        error ->
            init:stop(2);
        ok ->
            init:stop(0)
    end.

run() ->
    Modules = [event_tests,
               search_tests,
               meeting_tests,
               presence_tests,
               user_tests,
               time_tests,
               file_tests,
               ctl_tests,
               infos_tests] ++
        case config:get(search) of
            solr ->
                [solr_tests];
            _ ->
                []
        end,
    Failed = lists:filter(fun(Module) ->
                                  io:format("> ~p~n", [Module]),
                                  case Module:test() of
                                      ok ->
                                          false;
                                      error ->
                                          true
                                  end
                          end, Modules),
    if
        length(Failed) > 0 ->
            error;
        true ->
            ok
    end.
