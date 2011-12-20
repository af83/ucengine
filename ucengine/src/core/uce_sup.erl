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
-module(uce_sup).

-behaviour(supervisor).

-include("uce.hrl").

-export([start_link/0, init/1, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PubSubSup = [{?PUBSUB_MODULE,
                  {?PUBSUB_MODULE, start_link, []},
                  permanent, brutal_kill, worker, [?PUBSUB_MODULE]}],
    Vhost = [{uce_vhost_sup, {uce_vhost_sup, start_link, []},
              permanent, infinity, supervisor, [uce_vhost_sup]}],
    {ok, {{one_for_all, 10, 10}, PubSubSup ++ Vhost}}.

start_child(ChildSpec) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, ChildSpec).
