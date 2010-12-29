-module(uce_app).
-author('victor.goya@af83.com').

-behaviour(application).

-export([start/0, start/2, stop/1]).

-include("uce.hrl").
-include_lib("yaws/include/yaws.hrl").

start() ->
    application:start(crypto),
    mnesia:create_schema([node()]),
    application:start(mnesia, permanent),
    application:start(inets),
    ibrowse:start(),
    application:start(uce).

start(_, _) ->
    case catch config:start_link("etc/uce.cfg") of
	{'EXIT', ReasonConfig} ->
	    throw({'EXIT', io_lib:format("Could not setup config: ~p", [ReasonConfig])});
	{error, ReasonConfig} ->
	    throw({'EXIT', io_lib:format("Could not setup config: ~p", [ReasonConfig])});
	ok ->
	    case uce_sup:start_link() of
		{ok, Pid} ->
		    case catch setup() of
			{'EXIT', Reason} ->
			    ?ERROR_MSG("~p~n", [lists:flatten(io_lib:format("~p", [Reason]))]),
			    {error, lists:flatten(Reason)};
			ok ->
			    {ok, Pid}
		    end;
		Error ->
		    {error, Error}
	    end
    end.

setup() ->
    case catch save_pid() of
	{'EXIT', ReasonPid} ->
	    throw({'EXIT', io_lib:format("Could not save pid in ~p: ~p", [config:get(pidfile), ReasonPid])});
	{error, ReasonPid} ->
	    throw({'EXIT', io_lib:format("Could not save pid in ~p: ~p", [config:get(pidfile), ReasonPid])});
	_ ->
	    nothing
    end,
    case catch triggers:init() of
	{'EXIT', ReasonTriggers} ->
	    throw({'EXIT', io_lib:format("Could not setup triggers: ~p", [ReasonTriggers])});
	{error, ReasonTriggers} ->
	    throw({'EXIT', io_lib:format("Could not setup triggers: ~p", [ReasonTriggers])});
	_ ->
	    nothing
    end,
    case catch setup_db() of
	{'EXIT', ReasonDB} ->
	    throw({'EXIT', io_lib:format("Could not setup database: ~p", [ReasonDB])});
	{error, ReasonDB} ->
	    throw({'EXIT', io_lib:format("Could not setup database: ~p", [ReasonDB])});
	_ ->
	    nothing
    end,
    case catch setup_acl() of
	{'EXIT', ReasonACL} ->
	    throw({'EXIT', io_lib:format("Could not setup ACL: ~p", [ReasonACL])});
	{error, ReasonACL} ->
	    throw({'EXIT', io_lib:format("Could not setup ACL: ~p", [ReasonACL])});
	_ ->
	    nothing
    end,
    case catch setup_bricks() of
	{'EXIT', ReasonBricks} ->
	    throw({'EXIT', io_lib:format("Could not setup Bricks: ~p", [ReasonBricks])});
	{error, ReasonBricks} ->
	    throw({'EXIT', io_lib:format("Could not setup Bricks: ~p", [ReasonBricks])});
	_ ->
	    nothing
    end,
    case catch setup_root() of
	{'EXIT', ReasonRoot} ->
	    throw({'EXIT', io_lib:format("Could not setup root account: ~p", [ReasonRoot])});
	{error, ReasonRoot} ->
	    throw({'EXIT', io_lib:format("Could not setup root account: ~p", [ReasonRoot])});
	_ ->
	    nothing
    end,
    case catch setup_controllers() of
	{'EXIT', ReasonControllers} ->
	    throw({'EXIT', io_lib:format("Could not setup controllers: ~p", [ReasonControllers])});
	{error, ReasonControllers} ->
	    throw({'EXIT', io_lib:format("Could not setup controllers: ~p", [ReasonControllers])});
	_ ->
	    nothing
    end,
    case catch setup_server() of
	{'EXIT', ReasonServer} ->
	    throw({'EXIT', io_lib:format("Could not setup HTTP server: ~p", [ReasonServer])});
	{error, ReasonServer} ->
	    throw({'EXIT', io_lib:format("Could not setup HTTP server: ~p", [ReasonServer])});
	_ ->
	    nothing
    end,
    ok.

stop(State) ->
    remove_pid(),
    State.

setup_acl() ->
    [[triggers:add(#uce_trigger{location='_',
				type=Type,
				action={{uce_acl, trigger},
					#uce_acl{object=Object,
						 action=Action,
						 conditions=Conditions}}})
      || {Object, Action, Conditions} <- Rules] || {Type, Rules} <- config:get(acl)].

setup_db() ->
    DBBackend = list_to_atom(atom_to_list(config:get(db)) ++ "_db"),
    DBBackend:init(config:get(config:get(db))).

setup_bricks() ->
    lists:map(fun({Name, Token}) ->
		      uce_user:add(#uce_user{uid=Name,
					     auth="token",
					     credential=Token,
					     metadata=[]}),
		      % delete me
		      uce_presence:add(#uce_presence{sid=Token,
						     uid=Name,
						     auth="token",
						     metadata=[]}),
		      uce_acl:add(#uce_acl{uid=Name,
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
	    uce_user:add(#uce_user{uid=Uid,
				   auth=Auth,
				   credential=Credential,
				   metadata=Metadata}),

	    uce_acl:add(#uce_acl{uid=Uid,
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
                   doc_controller,
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
