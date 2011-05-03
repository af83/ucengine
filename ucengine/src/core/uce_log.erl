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
-module(uce_log).

-export([debug/3, info/3, warning/3, error/3, critical/3]).

debug(Format, ML, Args) ->
    log(debug, Format, ML, Args).

info(Format, ML, Args) ->
    log(info, Format, ML, Args).

warning(Format, ML, Args) ->
    log(warning, Format, ML, Args).

error(Format, ML, Args) ->
    log(error, Format, ML, Args).

critical(Format, ML, Args) ->
    log(critical, Format, ML, Args).


current_level(debug) ->
    1;
current_level(info) ->
    2;
current_level(warning) ->
    3;
current_level(error) ->
    4;
current_level(critical) ->
    5.

log(Level, Format, ML, Args) ->
    log(current_level(Level), current_level(config:get(log_level)), Format, ML, Args).

log(Level, ConfigLevel, Format, [Module, Line], Args) when Level >= ConfigLevel ->
    uce_logger:log(Level, Module, Line, Format, Args);
log(_Level, _Configlevel, _Format, [_Module, _Line], _Args) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

current_level_test() ->
    lists:foreach(fun({Level, Expected}) ->
                          ?assertEqual(Expected, current_level(Level))
                  end, [{debug, 1}, {info, 2}, {warning, 3}, {error, 4}, {critical, 5}]).

-endif.
