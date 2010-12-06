-module(uce_async).

-author('victor.goya@af83.com').

-export([listen/9]).

-include("uce.hrl").
-include("uce_async.hrl").

listen(Location, Search, From, Types, Uid, Start, End, Parent, Socket) ->
    ?PUBSUB_MODULE:subscribe(self(), Location, Search, From, Types, Uid, Start, End, Parent),
    Res = receive
	      {message, _} ->
		  case uce_event:list(Location, Search, From, Types, Uid, Start, End, Parent) of
		      {error, Reason} ->
			  {error, Reason};
		      Events ->
			  JSONEvent = mochijson:encode({struct,
							[{result,
							  event_helpers:to_json(Events)}]}),
			  yaws_api:stream_process_deliver_final_chunk(Socket, list_to_binary(JSONEvent)),			  
			  ok
		  end;
	      {error, Reason} ->
		  {error, Reason};
	      _ ->
		  ok
	  end,
    ?PUBSUB_MODULE:unsubscribe(self()),
    Res.
