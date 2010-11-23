-module(uce_event_erlang_search).

-author('victor.goya@af83.com').

-export([add/1, list/7]).

-include("uce.hrl").

-define(EVENT_DBMOD, (fun() ->
			      list_to_atom("uce_event_" ++ atom_to_list(config:get(db)))
		      end())).

add(_) ->
    ok.

search_value(_, []) ->
    true;
search_value(Value, [Word|Words]) ->
    case string:str(Value, Word) of
	0 ->
	    false;
	_ ->
	    search_value(Value, Words)
    end. 
search_metadata([], _) ->
    false;
search_metadata([{_, Value}|Tail], Words) ->
    case search_value(Value, Words) of
	true ->
	    true;
	false ->
	    search_metadata(Tail, Words)
    end.
search(Events, Words) ->
    lists:filter(fun(#uce_event{metadata=Metadata}) ->
			 search_metadata(Metadata, Words)
		 end,
		 Events).

list(Location, Search, From, Type, Start, End, Parent) ->
    case ?EVENT_DBMOD:list(Location, From, Type, Start, End, Parent) of
	{error, Reason} ->
	    {error, Reason};
	Events ->
	    search(Events, Search)
    end.
