-module(amqp_pubsub).

-behaviour(gen_server).

-author('victor.goya@af83.com').

-export([start_link/0,
	 init/1,
	 publish/4,
	 subscribe/9,
	 unsubscribe/1,
	 handle_messages/3,
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
	    {ok, Connection} =
		amqp_connection:start(network, #amqp_params{username=list_to_binary(Username),
							    password=list_to_binary(Password),
							    host=Host,
							    port=Port}),

	    {ok, Channel} = amqp_connection:open_channel(Connection),
	    {ok, {Channel}};
	_ ->
	    {error, bad_configuration}
    end.

publish(Location, Type, From, Id) ->
    case Location of
	[""] ->
	    gen_server:call(?MODULE, {publish, Location, Type, From, Id}),
	    gen_server:call(?MODULE, {publish, Location, '_', From, Id});
	[_] ->
	    gen_server:call(?MODULE, {publish, Location, Type, From, Id}),
	    gen_server:call(?MODULE, {publish, Location, '_', From, Id}),
	    gen_server:call(?MODULE, {publish, ["", ""], Type, From, Id}),
	    gen_server:call(?MODULE, {publish, ["", ""], '_', From, Id})
    end.

subscribe(Pid, Location, Search, From, Types, Uid, _Start, _End, _Parent) ->
    [gen_server:cast(?MODULE, {subscribe, Location, Uid, Search, Type, From, Pid}) || Type <- Types].

unsubscribe(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

params_to_queue([Meeting], Type) ->
    QueueLocation = yaws_api:url_encode(Meeting),
    QueueType = case Type of
		    '_' ->
			[];
		    _ ->
			yaws_api:url_encode(Type)
		end,
    list_to_binary(QueueLocation ++ "|" ++ QueueType).

handle_messages(Channel, Queue, Pid) ->
    receive
	#'basic.consume_ok'{} ->
	    ?MODULE:handle_messages(Channel, Queue, Pid);
	#'basic.cancel_ok'{} ->
	    ok;
	{#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload=Id}} ->
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	    Pid ! {message, Id}
    end.

handle_call({publish, Location, Type, _From, Id}, _From, {Channel} = State) ->
    Queue = params_to_queue(Location, Type),

    Exchange = <<>>,
    Publish = #'basic.publish'{exchange = Exchange, routing_key = Queue},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = list_to_binary(Id)}),
    {reply, ok, State}.

handle_cast({subscribe, Location, _Uid, _Search, Type, _From, Pid}, {Channel} = State) ->
    Queue = params_to_queue(Location, Type),

    % Create queue
    Declare = #'queue.declare'{queue = Queue},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),

    % Subscribe to it
    Sub = #'basic.consume'{queue = Queue},
    Listener = spawn_link(?MODULE, handle_messages, [Channel, Queue, Pid]),
    case catch amqp_channel:subscribe(Channel, Sub, Listener) of
	#'basic.consume_ok'{consumer_tag = Tag} ->
	    put(Pid, Tag);
	Error ->
	    ?ERROR_MSG("~p: ~p~n", [?MODULE, Error])	    
    end,
    {noreply, State};

handle_cast({unsubscribe, Pid}, {Channel} = State) ->
    case get(Pid) of
	undefined ->
	    nothing;
	Tag ->
	    amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag})
    end,
    {noreply, State}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.
