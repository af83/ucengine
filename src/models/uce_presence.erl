-module(uce_presence).

-author('tbomandouki@af83.com').

-export([add/1, get/1, delete/1, update/1, exists/1, all/0, joinMeeting/2, leaveMeeting/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_presence{sid=[]}=Presence) ->
    add(Presence#uce_presence{sid=utils:random()});
add(#uce_presence{last_activity=undefined}=Presence) ->
    add(Presence#uce_presence{last_activity=utils:now()});
add(#uce_presence{}=Presence) ->
    ?DB_MODULE:add(Presence).

get(Sid) ->
    ?DB_MODULE:get(Sid).

all() ->
    ?DB_MODULE:all().

delete(Sid) ->
    case ?MODULE:exists(Sid) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:delete(Sid)
    end.

update(#uce_presence{}=Presence) ->
    case ?MODULE:get(Presence#uce_presence.sid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Presence)
    end.

exists(Sid) ->
    case ?MODULE:get(Sid) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.

joinMeeting(Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Sid),
    Meetings = Record#uce_presence.meetings ++ [Meeting],
    ?DB_MODULE:update(Record#uce_presence{meetings=Meetings}).

leaveMeeting(Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Sid),
    Meetings = del_entry(Record#uce_presence.meetings, Meeting),
    ?DB_MODULE:update(Record#uce_presence{meetings=Meetings}).

del_entry([Entry], Entry) ->
    [];
del_entry([Entry, Tl], Entry) ->
    Tl;
del_entry([Hd, Tl], Entry) ->
    [Hd] ++ del_entry(Tl, Entry);
del_entry([], _Entry) ->
    []. 
