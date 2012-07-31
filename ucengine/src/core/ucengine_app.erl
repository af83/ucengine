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
-module(ucengine_app).

-behaviour(application).

-export([start/0]).

%% application callback
-export([start/2, stop/1]).

-include("uce.hrl").

start() ->
    application:start(ucengine).

start(_, _) ->
    start_apps([sasl, crypto, metrics, gproc, ibrowse]),
    mnesia:create_schema([node()|nodes()]),
    application:start(mnesia, permanent),
    error_logger:tty(false),

    Arguments = init:get_arguments(),
    [[ConfigurationPath]] = utils:get(Arguments, [c], [["etc/uce.cfg"]]),
    ok = config:init(ConfigurationPath),
    case uce_sup:start_link() of
        {ok, Pid} ->
            setup(),
            {ok, Pid};
        Error ->
            {error, Error}
    end.

start_apps([]) ->
    ok;
start_apps([App|Apps]) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            io:format("error ~p~n", [Error])
    end,
    start_apps(Apps).

setup() ->
    save_pid(),
    setup_routes(),
    ok.

stop(State) ->
    remove_pid(),
    State.

setup_routes() ->
    routes:init().

save_pid() ->
    Pid = os:getpid(),
    PidFile = config:get(pidfile),
    {ok, PidFileId} = file:open(PidFile, [read, write]),
    io:fwrite(PidFileId, "~s", [Pid]),
    file:close(PidFileId),
    ok.

remove_pid() ->
    PidFile = config:get(pidfile),
    file:delete(PidFile),
    ok.
