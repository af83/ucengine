%%
%%  U.C.Engine - Unified Colloboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(uce_presence).

-author('tbomandouki@af83.com').

-export([add/2, get/2, delete/2, update/2, exists/2, all/1, joinMeeting/3, leaveMeeting/3]).

-include("uce.hrl").
-include("uce_models.hrl").

add(Domain, #uce_presence{sid=[]}=Presence) ->
    add(Domain, Presence#uce_presence{sid=utils:random()});
add(Domain, #uce_presence{last_activity=undefined}=Presence) ->
    add(Domain, Presence#uce_presence{last_activity=utils:now()});
add(Domain, #uce_presence{}=Presence) ->
    ?DB_MODULE:add(Domain, Presence).

get(Domain, Sid) ->
    ?DB_MODULE:get(Domain, Sid).

all(Domain) ->
    ?DB_MODULE:all(Domain).

delete(Domain, Sid) ->
    case ?MODULE:exists(Domain, Sid) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:delete(Domain, Sid)
    end.

update(Domain, #uce_presence{}=Presence) ->
    case ?MODULE:get(Domain, Presence#uce_presence.sid) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Domain, Presence)
    end.

exists(Domain, Sid) ->
    case ?MODULE:get(Domain, Sid) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.

joinMeeting(Domain, Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Domain, Sid),
    case lists:member(Meeting, Record#uce_presence.meetings) of
        true -> {ok, updated};
        _ ->
            Meetings = Record#uce_presence.meetings ++ [Meeting],
            ?DB_MODULE:update(Domain, Record#uce_presence{meetings=Meetings})
    end.

leaveMeeting(Domain, Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Domain, Sid),
    Meetings = del_entry(Record#uce_presence.meetings, Meeting),
    ?DB_MODULE:update(Domain, Record#uce_presence{meetings=Meetings}).

del_entry([Entry], Entry) ->
    [];
del_entry([Entry | Tl], Entry) ->
    Tl;
del_entry([Hd | Tl], Entry) ->
    [Hd] ++ del_entry(Tl, Entry);
del_entry([], _Entry) ->
    []. 
