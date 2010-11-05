-module(uce_file).

-author('victor.goya@af83.com').

-export([add/1, list/1, get/1, delete/1]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_file{location=Location, name=Name} = File) ->
    case meeting_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    Id = case re:run(Name, "([^/]+)\\.([^/]+)$ ?", [{capture, all, list}]) of
		     {match, [_, BareName, Extension]} ->
			 BareName ++ "_" ++ utils:random() ++ "." ++ Extension;
		     _ ->
			 Name ++ "_" ++ utils:random()
		 end,
	    case ?DBMOD:add(File#uce_file{id=Id}) of
		{error, Reason} ->
		    {error, Reason};
		_ ->
		    Id
	    end
    end.

list(Location) ->
    case meeting_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    ?DBMOD:list(Location)
    end.

get(Id) ->
    ?DBMOD:get(Id).

delete(Id) ->
    case ?MODULE:get(Id) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:delete(Id)
    end.
