-module(amqp_pubsub).

-author('victor.goya@af83.com').

-export([init/1,
	 publish/4,
	 subscribe/9,
	 unsubscribe/6]).

-include("uce.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init(Config) ->
    case utils:get(Config, [username, password, host, port]) of
	[Username, Password, Host, Port] ->
	    Channel = amqp_connection:start_network(#amqp_params{username=Username,
								 password=Password,
								 host=Host,
								 port=Port}),
	    ok;
	_ ->
	    {error, bad_configuration}
    end.

publish(Location, Type, From, Message) ->
    nothing.

subscribe(Location, Search, From, Types, Uid, _Start, _End, _Parent, Pid) ->
    nothing.

unsubscribe(Location, Uid, Search, Type, From, Pid) ->
    nothing.
