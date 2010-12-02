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
					?ERROR_MSG("Error in event wait: ~p~n", [Reason])
				end,
				yaws_api:stream_process_end(Socket, YawsPid);
			    {discard, YawsPid}->
				yaws_api:stream_process_end(Socket, YawsPid);
			    _ ->
				nothing
			end
		end),
    {streamcontent_from_pid, "application/json", Pid}.
