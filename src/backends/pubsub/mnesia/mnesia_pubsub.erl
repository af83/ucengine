-module(mnesia_pubsub).

-behaviour(gen_server).

-author('victor.goya@af83.com').

-export([init/1,
	 start_link/0,
	 publish/4,
	 subscribe/9,
	 unsubscribe/6,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-include("uce.hrl").

-record(uce_mnesia_pubsub, {location, uid, search, type, from, pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    mnesia:create_table(uce_mnesia_pubsub,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, uce_mnesia_pubsub)}]),
    {ok, {}}.

publish(Location, Type, From, Message) ->
    gen_server:call(?MODULE, {publish, Location, Type, From, Message}).

subscribe(Location, Search, From, Types, Uid, _Start, _End, _Parent, Pid) ->
    [gen_server:cast(?MODULE, {subscribe, Location, Uid, Search, Type, From, Pid}) || Type <- Types].

unsubscribe(Location, Uid, Search, Type, From, Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Location, Uid, Search, Type, From, Pid}).

get_subscribers(Location, Type, From) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_mnesia_pubsub{
							   location=Location,
							   uid='_',
							   search='_',
							   type='_',
							   from='_',
							   pid='_'})
			    end) of
	{aborted, _} ->
	    {error, bad_parameters};
	{atomic, Subscribers} ->
	    lists:filter(fun(#uce_mnesia_pubsub{type=SubType, from=SubFrom}) ->
				 if
				     SubType == Type, SubFrom == From ->
					 true;
				     SubType == Type, SubFrom == '_' ->
					 true;
				     SubType == '_', SubFrom == From ->
					 true;
				     SubType == '_', SubFrom == '_' ->
					 true;
				     true ->
					 false
				 end
			 end,
			 Subscribers)
    end.

handle_call({publish, Location, Type, From, Message}, _From, {}) ->
    Return = case get_subscribers(Location, Type, From) of
		 {error, Reason} ->
		     {error, Reason};
		 Subscribers ->
		     [Subscriber#uce_mnesia_pubsub.pid ! {message, Message}
		      || Subscriber <- Subscribers],
		     ok
	     end,
    {reply, Return, {}}.

handle_cast({subscribe, Location, Uid, Search, Type, From, Pid}, {}) ->
    mnesia:transaction(fun() ->
			       mnesia:write(#uce_mnesia_pubsub{location=Location,
							       uid=Uid,
							       search=Search,
							       type=Type, 
							       from=From,
							       pid=Pid})
		       end),
    {noreply, {}};
handle_cast({unsubscribe, Location, Uid, Search, Type, From, Pid}, {}) ->
    mnesia:transaction(fun() ->
			       mnesia:delete(#uce_mnesia_pubsub{location=Location,
								uid=Uid,
								search=Search,
								type=Type,
								from=From,
								pid=Pid})
		       end),
    {noreply, {}}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.
