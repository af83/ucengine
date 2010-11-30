-module(uce_presence).

-author('tbomandouki@af83.com').

-export([add/1, get/1, delete/1, update/1, exists/1]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_presence{sid=[]}=Presence) ->
    add(Presence#uce_presence{sid=utils:random()});
add(#uce_presence{last_activity=undefined}=Presence) ->
    add(Presence#uce_presence{last_activity=utils:now()});
add(#uce_presence{}=Presence) ->
    ?DBMOD:add(Presence).

get(Sid) ->
    ?DBMOD:get(Sid).

delete(Sid) ->
    case ?MODULE:exists(Sid) of
	false ->
	    {error, not_found};
	true ->
	    ?DBMOD:delete(Sid)
    end.

update(#uce_presence{}=Presence) ->
    case ?MODULE:get(Presence#uce_presence.sid) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:update(Presence)
    end.

exists(Sid) ->
    case ?MODULE:get(Sid) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.
