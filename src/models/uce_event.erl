-module(uce_event).

-author('tbomandouki@af83.com').

-export([add/1, get/1, list/6, last/3, last/2]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_event{id=none}=Event) ->
    ?MODULE:add(Event#uce_event{id=utils:random()});
add(#uce_event{datetime=none}=Event) ->
    ?MODULE:add(Event#uce_event{datetime=utils:now()});
add(#uce_event{location=Location, type=Type, id=Id, from=From} = Event) ->
    case meeting_helpers:exists(Location) of
	true ->
	    case ?DBMOD:add(Event) of
		{error, Reason} ->
		    {error, Reason};
		_ ->
		    mnesia_pubsub:publish(Location, Type, From, Id),
		    case catch triggers:run(Location, Type, Event) of
			{error, Reason} ->
			    ?DEBUG("Error : ~p~n", [{error, Reason}]);
			{'EXIT', Reason} ->
			    ?DEBUG("Error : ~p~n", [{error, Reason}]);
			_ ->
			    nothing
		    end,
		    Id
	    end;
	false ->
	    {error, not_found}
    end.

get(Id) ->
    ?DBMOD:get(Id).

list(_, _, _, [], _, _) ->
    [];
list(Location, Search, From, '_', Start, End) ->
    ?MODULE:list(Location, Search, From, ['_'], Start, End);
list(Location, Search, From, [Type|Tail] = Types, Start, End) ->
    SearchEngine = ?SEARCH_ENGINE,
    if
	SearchEngine /= undefined, Search /= '_' ->
	    [Org, Meeting] = Location,
	    SearchEngine:search(Org, Meeting,
				case From of
				    '_' ->
					none;
				    _ ->
					From
				end
				, case Types of
				      ['_'] ->
					  none;
				      _ ->
					  Types
				  end
				, case Start of
				      0 ->
					  none;
				      _ ->
					  Start
				  end
				, case End of
				      infinity ->
					  none;
				      _ ->
					  End
				  end
				, Search);
	true ->
	    ?DBMOD:list(Location, Search, From, Type, Start, End) ++
		?MODULE:list(Location, Search, From, Tail, Start, End)
    end.

last(Location, Type) ->
    last(Location, '_', Type).

last(Location, From, Type) ->
    case ?MODULE:get(Location, From, Type, '_', '_', -1) of
	[R] -> R;
	_ -> none
    end.
