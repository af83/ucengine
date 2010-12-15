-module(uce_file).

-author('victor.goya@af83.com').

-export([add/1, list/1, get/1, delete/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_file{location=Location, name=Name} = File) ->
    case location_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    Id = case re:run(Name, "([^/]+)\\.([^/]+)$ ?", [{capture, all, list}]) of
		     {match, [_, BareName, Extension]} ->
			 BareName ++ "_" ++ utils:random() ++ "." ++ Extension;
		     _ ->
			 Name ++ "_" ++ utils:random()
		 end,
	    case ?DB_MODULE:add(File#uce_file{id=Id}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    {ok, Id}
	    end
    end.

list(Location) ->
    case location_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:list(Location)
    end.

get(Id) ->
    ?DB_MODULE:get(Id).

delete(Id) ->
    case ?MODULE:get(Id) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Id)
    end.
