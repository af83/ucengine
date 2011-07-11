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
-module(uce_vhost_user_sup).

-behaviour(supervisor).

-include("uce.hrl").

% External API
-export([start_link/1, start_child/2, terminate_child/2]).
% Supervisor API
-export([init/1]).

start_link(Domain) ->
    supervisor:start_link({local, uce_vhost_sup:name(Domain, "user")}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{uce_user, {uce_user, start_link, []},
            temporary, brutal_kill, worker, [uce_user]}]}}.

start_child(Domain, Args) ->
    supervisor:start_child(uce_vhost_sup:name(Domain, "user"), Args).

terminate_child(Domain, Pid) ->
    supervisor:terminate_child(uce_vhost_sup:name(Domain, "user"), Pid).
