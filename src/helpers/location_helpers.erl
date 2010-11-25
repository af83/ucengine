-module(location_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([exists/1]).

exists(Location) ->
    case Location of
	["", ""] ->
	    true;
	[Org, ""] ->
	    case uce_org:get(Org) of
		{error, _} ->
		    false;
		_ ->
		    true
	    end;	    
	[Org, Meeting] ->
	    case uce_meeting:get([Org, Meeting]) of
		{error, _} ->
		    false;
		_ ->
		    true
	    end
    end.

