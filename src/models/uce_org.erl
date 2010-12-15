-module(uce_org).

-author('victor.goya@af83.com').

-export([add/1, update/1, get/1, delete/1, list/0, exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_org{}=Org) ->
    case ?MODULE:get(Org#uce_org.name) of
	{error, not_found} ->
	    ?DB_MODULE:add(Org);
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    {error, conflict}
    end.


update(#uce_org{}=Org) ->
    case ?MODULE:get(Org#uce_org.name) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Org)
    end.

get(Name) ->
    ?DB_MODULE:get(Name).

delete(Name) ->
    case ?MODULE:get(Name) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:delete(Name)
    end.

list() ->
    ?DB_MODULE:list().

exists(Name) ->
    case ?MODULE:get(Name) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.
