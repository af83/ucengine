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
-module(uce_app).
-author('victor.goya@af83.com').

-behaviour(application).

-export([start/0]).

%% application callback
-export([start/2, stop/1]).

-include("uce.hrl").
-include_lib("yaws/include/yaws.hrl").


start() ->
    application:start(uce).

start(_, _) ->
    application:start(crypto),
    mnesia:create_schema([node()|nodes()]),
    application:start(mnesia, permanent),
    application:start(inets),
    ibrowse:start(),

    Arguments = init:get_arguments(),
    [[ConfigurationPath]] = utils:get(Arguments, [c], [["etc/uce.cfg"]]),
    ok = config:start_link(ConfigurationPath),
    case uce_sup:start_link() of
        {ok, Pid} ->
            setup(),
            {ok, Pid};
        Error ->
            {error, Error}
    end.

setup() ->
    save_pid(),
    setup_db(),
    setup_bricks(),
    setup_root(),
    setup_controllers(),
    setup_server(),
    ok.

stop(State) ->
    remove_pid(),
    State.

setup_db() ->
    DBBackend = list_to_atom(atom_to_list(config:get(db)) ++ "_db"),
    DBBackend:init(config:get(config:get(db))).

setup_bricks() ->
    lists:map(fun({Name, Token}) ->
                      catch uce_user:add(#uce_user{id={Name, config:get(default_domain)},
                                             auth="token",
                                             credential=Token,
                                             metadata=[]}),
                      catch uce_acl:add(#uce_acl{user={Name, config:get(default_domain)},
                                           action="all",
                                           object="all",
                                           conditions=[]})
              end,
              config:get(bricks)).

setup_root() ->
    case utils:get(config:get(admin),
                   [uid, auth, credential, metadata],
                   [none, none, none, []]) of
        [Uid, Auth, Credential, Metadata]
          when is_list(Uid) and is_list(Auth) and is_list(Credential) ->
            catch uce_user:add(#uce_user{id={Uid, config:get(default_domain)},
                                         auth=Auth,
                                         credential=Credential,
                                         metadata=Metadata}),
            catch uce_acl:add(#uce_acl{user={Uid, config:get(default_domain)},
                                       action="all",
                                       object="all",
                                       conditions=[]});
        _ ->
            ?ERROR_MSG("Invalid or inexistent admin account~n", [])
    end.

setup_controllers() ->
    lists:foreach(fun(Controller) ->
                          [routes:set(Route) || Route <- Controller:init()]
                  end,
                  %% TODO: make this more generic
                  [user_controller,
                   presence_controller,
                   meeting_controller,
                   event_controller,
                   file_controller,
                   acl_controller,
                   time_controller,
                   infos_controller
                  ]).

setup_server() ->
    yaws:start_embedded(config:get(root),
                        [{servername, "ucengine"},
                         {listen, {0,0,0,0}},
                         {port, config:get(port)},
                         {appmods, [{"/api/" ++ config:get(version), appmod_uce}]}],
                        [{auth_log, false},
                         {access_log, false},
                         {copy_errlog, false},
                         {debug, false},
                         {copy_error_log, false},
                         {max_connections, nolimit}]),
    {ok, GConf, SConfs} = yaws_api:getconf(),
    yaws_api:setconf(GConf#gconf{cache_refresh_secs=config:get(cache_refresh)}, SConfs).

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
