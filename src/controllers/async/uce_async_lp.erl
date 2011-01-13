-module(uce_async_lp).

-author('victor.goya@af83.com').

-export([wait/9]).

-include("uce.hrl").

wait(Location, Search, From, Types, Uid, Start, End, Parent, Socket) ->
    Pid = spawn(fun() ->
			receive
			    {ok, YawsPid} ->
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
					Error = case http_helpers:error_to_code(Reason) of
						    500 ->
							unexpected_error;
						    _ ->
							Reason
						end,
					JSONError =
					    list_to_binary(mochijson:encode({struct,
									     [{error, Error}]})),
					yaws_api:stream_process_deliver_final_chunk(Socket, JSONError)
				end,
				yaws_api:stream_process_end(Socket, YawsPid);
			    {discard, YawsPid}->
				yaws_api:stream_process_end(Socket, YawsPid)
			end
		end),
    {streamcontent_from_pid, "application/json", Pid}.
