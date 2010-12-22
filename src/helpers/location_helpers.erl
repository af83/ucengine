-module(location_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([exists/1]).

exists(Location) ->
    case Location of
	[""] ->
	    true;
	[Meeting] ->
	    case uce_meeting:get([Meeting]) of
		{error, _} ->
		    false;
		_ ->
		    true
	    end
    end.

