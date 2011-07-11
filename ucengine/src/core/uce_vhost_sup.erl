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

-include("uce.hrl").

% External API
-export([start_link/0, name/2]).
% Supervisor API
-export([init/1]).

name(Domain, Server) ->
    list_to_atom(lists:concat([Server, "_", Domain])).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hosts = config:get(hosts),
    Vhosts = vhost_supervise(Hosts),
    {ok, {{one_for_one, 10, 10}, Vhosts}}.

vhost_supervise([]) ->
    [];
vhost_supervise([{Domain, _HostConfig}|R]) ->
    [{name(Domain, "vhost"),
      {uce_vhost, start_link, [Domain]},
      permanent, brutal_kill, worker, [uce_vhost]},
    {name(Domain, "timeout"),
     {uce_timeout, start_link, [Domain]},
     permanent, brutal_kill, worker, [uce_timeout]},
    {name(Domain, "presence"),
     {uce_vhost_user_sup, start_link, [Domain]},
     permanent, brutal_kill, supervisor, [uce_vhost_user_sup]}] ++ vhost_supervise(R).


%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

name_test() ->
    ?assertEqual(vhost_localhost, name(localhost, "vhost")),
    ?assertEqual('vhost_example.com', name('example.com', "vhost")).

-endif.
