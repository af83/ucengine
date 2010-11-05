-module(triggers).

-include("uce.hrl").

-author('victor.goya@af83.com').

-export([init/0, terminate/0, add/1, list/2, run/2, run/3]).

init() ->
    mnesia:create_table(uce_trigger,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, uce_trigger)}]).

terminate() ->
    ok.

add(#uce_trigger{} = Trigger) ->
    mnesia:dirty_write(Trigger).

list(Location, Type) ->
    lists:filter(fun(#uce_trigger{location=TriggerLocation}) ->
			 case TriggerLocation of
			     '_' ->
				 true;
			     Location ->
				 true;
			     _ ->
				 false
			 end
		 end,
		 lists:filter(fun(#uce_trigger{type=TriggerType}) ->
				      case TriggerType of
					  '_' ->
					      true;
					  Type ->
					      true;
					  _ ->
					      false
				      end
			      end,
			      ets:tab2list(uce_trigger))).
run([], _) ->
    [];
run([Trigger|Tl], Event) ->
    {Action, Params} = Trigger#uce_trigger.action,
    Result = case Action of
		 {url, Url} ->
		     case catch httpc:request(post,
					      {Url,
					       [],
					       [],
					       "{\"event\":" ++
						   lists:flatten(event:to_json(Event)) ++
						   "}"
					      },
					      [{timeout, ?TIMEOUT}],
					      []) of
			 {'EXIT', _} ->
			     case http:request(post,
					       {Url,
						[],
						[],
						"{\"event\":" ++
						    lists:flatten(event:to_json(Event)) ++
						    "}"
					       },
					       [{timeout, ?TIMEOUT}],
					       []) of
				 {'EXIT', Reason} -> {error, Reason};
				 _ -> ok
			     end;
			 _ ->
			     ok
		     end;
		 {Module, Method} ->
		     case catch Module:Method(Event, Params) of
			 ok ->
			     ok;
			 {_, Reason} ->
			     {error, Reason};
			 _ ->
			     {error, unexpected_error}
		     end
	     end,
    [{Action, Result}] ++ ?MODULE:run(Tl, Event).

run(Location, Type, Event) ->
    ?MODULE:run(triggers:list(Location, Type), Event).
