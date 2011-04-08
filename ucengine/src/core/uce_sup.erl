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
-module(uce_sup).

-behaviour(supervisor).

-include("uce.hrl").

-export([start_link/1, init/1]).

start_link(ConfigPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfigPath]).

init(ConfigPath) ->
    Config = [{config,
               {config, start_link, [ConfigPath]},
               permanent, brutal_kill, worker, [config]}],
    Routes = [{routes,
               {routes, start_link, []},
               permanent, brutal_kill, worker, [routes]}],
    Timeout = [{timeout,
                {timeout, start_link, []},
                permanent, brutal_kill, worker, [timeout]}],
    PubSubSup = [{?PUBSUB_MODULE,
                  {?PUBSUB_MODULE, start_link, []},
                  permanent, brutal_kill, worker, [?PUBSUB_MODULE]}],
    Vhost = [{uce_vhost_sup, {uce_vhost_sup, start_link, []},
              permanent, infinity, supervisor, [uce_vhost_sup]}],
    {ok, {{one_for_one, 10, 10},
          Config ++ Routes ++ Timeout ++ PubSubSup ++ Vhost}}.
