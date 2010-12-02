-module(uce_async_ws).

-author('victor.goya@af83.com').

-export([wait/9]).

-include("uce.hrl").

wait(Location, Search, From, Types, Uid, Start, End, Parent, Socket) ->
    Pid = spawn(fun() ->
			receive
			    {ok, WebSocket} ->
				case uce_async:listen(Location,
						      Search,
						      From, 
						      Types, 
						      Uid,
						      Start,
						      End,
						      Parent,
					    Socket) of
				    ok ->
					nothing;
				    {error, Reason} ->
					?ERROR_MSG("Error in event wait: ~p~n", [Reason])
				end,
				yaws_api:stream_process_end(Socket, WebSocket);
			    _ ->
				nothing
			end
		end),
    {websocket, Pid, passive}.
