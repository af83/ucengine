-module(event_controller).

-export([init/0, list/3, add/3]).

-include("uce.hrl").
-include("yaws_api.hrl").

init() ->
    {event, [#uce_route{method='GET',
			  regexp="/event/?([^/]+)?/?([^/]+)?/?",
			  callbacks=[{presence_controller, check,
				      ["uid", "sid"],
				      [required, required],
				      [string, string]},
				     {?MODULE, list,
				      ["uid",
				       "search",
				       "type",
				       "from",
				       "start",
				       "end",
				       "count",
				       "page",
				       "order",
				       "_async"],
				      [required, '_', '_', '_', 0, infinity, infinity, 1, asc, "no"],
				      [string,
				       [string, atom],
				       [string, atom],
				       [string, atom],
				       integer,
				       [integer, atom],
				       [integer, atom],
				       integer,
				       atom,
				       string]}]},
	     
	     #uce_route{method='PUT',
			  regexp="/event/?([^/]+)?/?([^/]+)?/?",
			  callbacks=[{presence_controller, check,
				      ["uid", "sid"],
				      [required, required],
				      [string, string]},
				     {?MODULE, add,
				      ["uid", "type", "metadata"],
				      [required, required, []],
				      [string, string, dictionary]}]}
	    ]}.

listen(Location, EUid, Search, Type, From, Socket) ->
    mnesia_pubsub:subscribe(Location, EUid, Search, Type, From, self()),
    Res = receive
	      {message, Id} ->
		  case uce_event:get(Id) of
		      {error, Reason} ->
			  ?DEBUG("Pubsub: unknown message ID: ~s (~p)~n", [Id, Reason]),
			  {error, Reason};
		      Event ->
			  JSONEvent = mochijson:encode({struct,
							[{result,
							  event_helpers:to_json(Event)}]}),
			  yaws_api:stream_process_deliver_final_chunk(Socket, list_to_binary(JSONEvent)),
			  
			  ok
		  end;
	      {error, Reason} ->
		  {error, Reason};
	      _ ->
		  ok
	  end,
    mnesia_pubsub:unsubscribe(Location, EUid, Search, Type, From, self()),
    Res.

wait(Location, EUid, Search, Type, From, Start, Socket) ->
    Pid = spawn(fun() ->
			receive
			    {ok, YawsPid} ->
				case listen(Location, EUid, Search, Type, From, Socket) of
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

list([], Match, Arg) ->
    ?MODULE:list(['_', '_'], Match, Arg);
list([Org], Match, Arg) ->
    ?MODULE:list([Org, '_'], Match, Arg);
list(Location, [EUid, Search, Type, From, Start, End, Count, Page, Order, Async], Arg) ->
    case uce_acl:check(EUid, "event", "add", [{"location", Location},
						{"from", From}]) of
	true ->
	    Types = case Type of
			'_' ->
			    ['_'];
			_ ->
			    string:tokens(Type, ",")
		    end,
	    Keywords = case Search of
			   '_' ->
			       '_';
			   _ ->
			       string:tokens(Search, " ")
		       end,
	    case uce_event:list(Location, Keywords, From, Types, Start, End) of
		{error, Reason} ->
		    {error, Reason};
		[] ->
		    case Async of
			"no" ->
			    json_helpers:json(event_helpers:to_json([]));
			"lp" ->
			    wait(Location, EUid, Keywords, Type, From, Start, Arg#arg.clisock);
			_ ->
			    {error, bad_parameters}
		    end;
		Events ->
		    case helpers:paginate(event_helpers:sort(Events), Count, Page, Order) of
			{error, Reason} ->
			    {error, Reason};
			EventPage ->
			    json_helpers:json(event_helpers:to_json(EventPage))
		    end
	    end;
	false ->
	    {error, unauthorized}
    end.

add(Location, [EUid, Type, Metadata], _) ->
    case uce_acl:check(EUid, "event", "add", [{"location", Location}, {"type", Type}]) of
	true ->
	    case uce_event:add(#uce_event{location=Location,
					      from=EUid,
					      type=Type,
					      metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		Id ->
		    json_helpers:created(Id)
	    end;
	false ->
	    {error, unauthorized}
    end.
