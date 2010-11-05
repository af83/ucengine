-module(config).

-author("victor.goya@af83.com").

-behaviour(gen_server).

-export([start/1,
	 init/1,
	 set/2,
	 get/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

start(Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    case file:consult(Path) of
	{ok, Configs} ->
	    lists:foreach(fun({Key, Value}) ->
				  config:set(Key, Value)
			  end,
			  Configs);
	_ ->
	    {error, parsing_error}
    end.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).
set(Key, Value) ->
    gen_server:cast(?MODULE, {set, {Key, Value}}).

init([]) ->
    {ok, {ets:new(uce_config, [set, public, {keypos, 1}])}}.

handle_call({get, Key}, _From, {DB}) ->
    Reply = case ets:lookup(DB, Key) of
		[{Key, Value}] ->
		    Value;
		_ ->
		    undefined
	    end,
    {reply, Reply, {DB}}.

handle_cast({set, {Key, Value}}, {DB}) ->
    ets:insert(DB, {Key, Value}),
    {noreply, {DB}}.

handle_info(_Info, State) ->
    {reply, State}.

code_change(_,State,_) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
