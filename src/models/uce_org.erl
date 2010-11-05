-module(uce_org).

-author('victor.goya@af83.com').

-export([add/1, update/1, get/1, delete/1, list/0]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_org{}=Org) ->
    case ?MODULE:get(Org#uce_org.name) of
	{error, not_found} ->
	    ?DBMOD:add(Org);
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    {error, conflict}
    end.


update(#uce_org{}=Org) ->
    case ?MODULE:get(Org#uce_org.name) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:update(Org)
    end.

get(Name) ->
    ?DBMOD:get(Name).

delete(Name) ->
    case ?MODULE:get(Name) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:delete(Name)
    end.

list() ->
    ?DBMOD:list().
