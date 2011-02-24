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
-module(uce_vhost_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hosts = config:get(hosts),
    Vhosts = vhost_supervise(Hosts),
    {ok, {{one_for_one, 10, 10}, Vhosts}}.

vhost_supervise([]) ->
    [];
vhost_supervise([{Domain, _HostConfig}|R]) ->
    [{uce_vhost:name(Domain),
      {uce_vhost, start_link, [Domain]},
      permanent, brutal_kill, worker, [uce_vhost]}|vhost_supervise(R)].
