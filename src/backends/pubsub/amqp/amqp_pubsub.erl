-module(amqp_pubsub).

-behaviour(gen_server).

-author('victor.goya@af83.com').

-export([start_link/0,
	 init/1,
	 publish/4,
	 subscribe/9,
	 unsubscribe/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-include("uce.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case utils:get(config:get(amqp), [username, password, host, port]) of
	[Username, Password, Host, Port] ->
	    io:format("PLOP~n"),
	    Connection = amqp_connection:start(network, #amqp_params{username=list_to_binary(Username),
								     password=list_to_binary(Password),
								     host=Host,
								     port=Port}),
	    io:format("CONN ~p~n", [Connection]),
	    Channel = amqp_connection:open_channel(Connection),
	    io:format("CHAN: ~p~n", [Channel]),
	    {ok, {Channel}};
	_ ->
	    {error, bad_configuration}
    end.

publish(Location, Type, From, Message) ->
    gen_server:call(?MODULE, {publish, Location, Type, From, Message}).

subscribe(Pid, Location, Search, From, Types, Uid, _Start, _End, _Parent) ->
    lists:foreach(fun(Type) ->
			  case Location of
			      [Org, Meeting] ->
				  gen_server:cast(?MODULE,
						  {subscribe, [Org, Meeting], Uid, Search, Type, From, Pid}),
				  gen_server:cast(?MODULE,
						  {subscribe, [Org], Uid, Search, Type, From, Pid}),
				  gen_server:cast(?MODULE,
						  {subscribe, [], Uid, Search, Type, From, Pid});
			      [Org] ->
				  gen_server:cast(?MODULE,
						  {subscribe, [Org], Uid, Search, Type, From, Pid}),
				  gen_server:cast(?MODULE,
						  {subscribe, [], Uid, Search, Type, From, Pid});
			      [] ->
				  gen_server:cast(?MODULE,
						  {subscribe, [], Uid, Search, Type, From, Pid})
			  end

		  end,
		  Types).

unsubscribe(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

params_to_queue(Location, Search, Type, From) ->
    QueueLocation = case Location of
			[Org, Meeting] ->
			    yaws_api:url_encode(Org) ++ "," ++ yaws_api:url_encode(Meeting);
			[Org] ->
			    yaws_api:url_encode(Org);
			[] ->
			    ""
		    end,
    QueueType = yaws_api:url_encode(Type),
    QueueLocation ++ "|" ++ QueueType.

handle_call({publish, Location, Type, From, Message}, _From, {Channel} = State) ->
    io:format("PLOP~n"),
    {reply, ok, State}.

handle_cast({subscribe, Location, Uid, Search, Type, From, Pid}, {Channel} = State) ->
    Sub = #'basic.consume'{queue = params_to_queue(Location, Search, Type, From)},
    Pid = spawn(fun() ->
			nothing
		end),
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, Pid),
    {noreply, State};

handle_cast({unsubscribe, Location, Uid, Search, Type, From, Pid}, {Channel} = State) ->
    {noreply, State}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.
