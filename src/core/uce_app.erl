-module(uce_app).
-author('victor.goya@af83.com').

-behaviour(application).

-export([start/0,
	 start/2,
	 stop/1]).

-include("uce.hrl").
-include("yaws.hrl").

start() ->
    application:start(emongo),
    application:start(crypto),
    mnesia:create_schema([node()]),
    application:start(mnesia, permanent),
    application:start(inets),
    application:start(uce),
    ok.

start(_, _) ->
    case uce_sup:start_link() of
	{ok, Pid} ->
	    config:start("etc/uce.cfg"),
	    save_pid(),

	    triggers:init(),

	    setup_db(),
	    setup_acl(),
	    setup_modules(),
	    setup_triggers(),
	    setup_bricks(),
	    setup_root(),
	    setup_controllers(),
	    setup_server(),

	    {ok, Pid};
	Error ->
	    {error, Error}
    end.

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

setup_modules() ->
    lists:map(fun({Module, Options}) ->
     		      Module:start(Options),
		      ?INFO_MSG("~p module loaded", [Module])
     	      end,
    	      config:get(modules)).    

setup_db() ->
    DBBackend = list_to_atom(atom_to_list(config:get(db)) ++ "_db"),
    DBBackend:init(config:get(config:get(db))).    

setup_triggers() ->
    lists:map(fun({Type, URL}) ->
		      triggers:add(#uce_trigger{location='_',
						type=Type,
						action={{url, URL}, []}})
	      end,
	      config:get(triggers)).

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
						     org=[],
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
	[EUid, Auth, Credential, Metadata]
	  when is_list(EUid) and is_list(Auth) and is_list(Credential) ->
	    uce_user:add(EUid, Auth, Credential, Metadata),
	    uce_acl:add(#uce_acl{uid=EUid,
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
		  [user_controller,
		   presence_controller,
		   meeting_controller,
		   event_controller,
		   file_controller,
		   acl_controller,
		   org_controller,
		   time_controller,
		   doc_controller]).

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
