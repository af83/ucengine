-module(uce_event).

-author('tbomandouki@af83.com').

-export([add/1, get/1, exists/1, list/8]).

-include("uce.hrl").
-include("uce_models.hrl").
-include("uce_async.hrl").

add(#uce_event{id=none}=Event) ->
    ?MODULE:add(Event#uce_event{id=utils:random()});
add(#uce_event{datetime=none}=Event) ->
    ?MODULE:add(Event#uce_event{datetime=utils:now()});
add(#uce_event{location=Location, type=Type, id=Id, from=From} = Event) ->
    case location_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    case ?DB_MODULE:add(Event) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    ?PUBSUB_MODULE:publish(Location, Type, From, Id),
		    ?SEARCH_MODULE:add(Event),
		    case catch triggers:run(Location, Type, Event) of
			{error, Reason} ->
			    ?DEBUG("Error : ~p~n", [{error, Reason}]);
			{'EXIT', Reason} ->
			    ?DEBUG("Error : ~p~n", [{error, Reason}]);
			_ ->
			    nothing
		    end,
		    {ok, Id}
	    end
    end.

get(Id) ->
    ?DB_MODULE:get(Id).

exists(Id) ->
    case Id of
	"" -> % "" is the root of the event hierarchy
	    true;
	_ ->
	    case ?MODULE:get(Id) of
		{error, _} ->
		    false;	       
		_ ->
		    true
	    end
    end.
		    
list(_, _, _, [], _, _, _, _) ->
    {ok, []};
list(Location, Search, From, '_', Uid, Start, End, Parent) ->
    ?MODULE:list(Location, Search, From, ['_'], Uid, Start, End, Parent);
list(Location, Search, From, [Type|Tail], Uid, Start, End, Parent) ->
    {ok, AllEvents} = case Search of
		    '_' ->
			?DB_MODULE:list(Location, From, Type, Start, End, Parent);
		    _ ->
			?SEARCH_MODULE:list(Location, Search, From, Type, Start, End, Parent)
		end,
    FilteredEvents = lists:filter(fun(#uce_event{to=To}) ->
					  if
					      To == "all" ->
						  true;
					      To == Uid ->
						  true;
					      true ->
						  false
					  end
				  end,
				  AllEvents),
    case ?MODULE:list(Location, Search, From, Tail, Uid, Start, End, Parent) of
	{error, Reason} ->
	    {error, Reason};
	{ok, RemainingEvents} ->
	    {ok, FilteredEvents ++ RemainingEvents}
    end.
