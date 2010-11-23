-module(uce_event).

-author('tbomandouki@af83.com').

-export([add/1, get/1, list/7, last/3, last/2]).

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
		    ?SEARCH_MOD:add(Event),
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

list(_, _, _, [], _, _, _) ->
    [];
list(Location, Search, From, '_', Start, End, Parent) ->
    ?MODULE:list(Location, Search, From, ['_'], Start, End, Parent);
list(Location, Search, From, [Type|Tail], Start, End, Parent) ->
    case Search of
	'_' ->
	    ?DBMOD:list(Location, From, Type, Start, End, Parent);
	_ ->
	    ?SEARCH_MOD:list(Location, Search, From, Type, Start, End, Parent)
    end ++ ?MODULE:list(Location, Search, From, Tail, Start, End, Parent).

last(Location, Type) ->
    last(Location, '_', Type).

last(Location, From, Type) ->
    case ?MODULE:get(Location, From, Type, '_', '_', -1) of
	[R] -> R;
	_ -> none
    end.
