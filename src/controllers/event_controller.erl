-module(event_controller).

-export([init/0, get/3, list/3, add/3]).

-include("uce.hrl").
-include("yaws_api.hrl").

init() ->
    [#uce_route{module="Events",
		method='GET',
		regexp="/event/([^/]+)/([^/]+)/([^/]+)",
		types=[any, any, event],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, get,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},

     #uce_route{module="Events",
		method='GET',
		regexp="/event/?([^/]+)?/?([^/]+)?/?",
		types=[org, meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
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
			     "parent",
			     "_async"],
			    [required, '_', '_', '_', 0, infinity, infinity, 1, asc, '_', "no"],
			    [string,
			     [string, atom],
			     [string, atom],
			     [string, atom],
			     integer,
			     [integer, atom],
			     [integer, atom],
			     integer,
			     atom,
			     [string, atom],
			     string],
			    [user, any, any, user, any, any, any, any, any, event, any]}]},
     
     #uce_route{module="Events",
		method='PUT',
		regexp="/event/?([^/]+)?/?([^/]+)?/?",
		types=[org, meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "type", "to", "parent", "metadata"],
			    [required, required, "all", "", []],
			    [string, string, string, string, dictionary],
			    [user, any, user, event, any]}]}].

get([_, _, Id], [Uid], _) ->
    case uce_acl:check(Uid, "event", "get", ["", ""], [{"id", Id}]) of
	false ->
	    {error, unauthorized};
	true ->
	    case uce_event:get(Id) of
		{error, Reason} ->
		    {error, Reason};
		#uce_event{to=To} = Event ->
		    if
			To == "all" ->
			    json_helpers:json(event_helpers:to_json(Event));
			To == Uid ->
			    json_helpers:json(event_helpers:to_json(Event));
			true ->
			    {error, unauthorized}
		    end
	    end
    end.

listen(Location, Uid, Search, Type, From, Socket) ->
    mnesia_pubsub:subscribe(Location, Uid, Search, Type, From, self()),
    Res = receive
	      {message, Id} ->
		  case uce_event:get(Id) of
		      {error, Reason} ->
			  ?DEBUG("Pubsub: unknown message ID: ~s (~p)~n", [Id, Reason]),
			  {error, Reason};
		      Event ->
			  JSONEvent = mochijson:encode({struct,
							[{result,
							  event_helpers:to_json([Event])}]}),
			  yaws_api:stream_process_deliver_final_chunk(Socket, list_to_binary(JSONEvent)),			  
			  ok
		  end;
	      {error, Reason} ->
		  {error, Reason};
	      _ ->
		  ok
	  end,
    mnesia_pubsub:unsubscribe(Location, Uid, Search, Type, From, self()),
    Res.

wait(Location, Uid, Search, Type, From, Start, Socket) ->
    Pid = spawn(fun() ->
			receive
			    {ok, YawsPid} ->
				case listen(Location, Uid, Search, Type, From, Socket) of
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
    ?MODULE:list(["", ""], Match, Arg);
list([Org], Match, Arg) ->
    ?MODULE:list([Org, ""], Match, Arg);
list(Location, [Uid, Search, Type, From, Start, End, Count, Page, Order, Parent, Async], Arg) ->
    case uce_acl:check(Uid, "event", "list", Location, [{"from", From}]) of
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
	    case uce_event:list(Location, Keywords, From, Types, Uid, Start, End, Parent) of
		{error, Reason} ->
		    {error, Reason};
		[] ->
		    case Async of
			"no" ->
			    json_helpers:json(event_helpers:to_json([]));
			"lp" ->
			    wait(Location, Uid, Keywords, Type, From, Start, Arg#arg.clisock);
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

add([], [Uid, Type, To, Parent, Metadata], Arg) ->
    ?MODULE:add(["", ""], [Uid, Type, To, Parent, Metadata], Arg);
add([Org], [Uid, Type, To, Parent, Metadata], Arg) ->
    ?MODULE:add([Org, ""], [Uid, Type, To, Parent, Metadata], Arg);
add(Location, [Uid, Type, To, Parent, Metadata], _) ->
    case uce_acl:check(Uid, "event", "add", Location, [{"type", Type},
							{"to", To}]) of
	true ->
	    case uce_event:add(#uce_event{location=Location,
					  from=Uid,
					  type=Type,
					  to=To,
					  parent=Parent,
					  metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		Id ->
		    json_helpers:created(Id)
	    end;
	false ->
	    {error, unauthorized}
    end.
