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

-export([add/1,
         all/0,
         get/1,
         delete/1,
         update/1,
         exists/1,
         check/2,
         assert/2,
         join/2,
         leave/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_presence{id=none}=Presence) ->
    add(Presence#uce_presence{id=utils:random()});
add(#uce_presence{last_activity=0}=Presence) ->
    add(Presence#uce_presence{last_activity=utils:now()});
add(#uce_presence{timeout=0}=Presence) ->
    add(Presence#uce_presence{timeout=config:get(presence_timeout)});
add(#uce_presence{}=Presence) ->
    ?DB_MODULE:add(Presence).

get(Id) ->
    ?DB_MODULE:get(Id).

all() ->
    ?DB_MODULE:all().

delete(Id) ->
    case ?MODULE:exists(Id) of
        false ->
            throw({error, not_found});
        true ->
            ?DB_MODULE:delete(Id)
    end.

update(#uce_presence{}=Presence) ->
    case ?MODULE:exists(Presence#uce_presence.id) of
        false ->
            throw({error, not_found});
        true ->
            ?DB_MODULE:update(Presence)
    end.

exists(Id) ->
    case catch ?MODULE:get(Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            true
    end.

assert(User, Sid) ->
    case check(User, Sid) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, unauthorized})
    end.
        
check(User, Sid) ->
    {ok, Record} = uce_presence:get(Sid),
    case Record#uce_presence.user of
        User ->
            uce_presence:update(Record#uce_presence{last_activity=utils:now()}),
            {ok, true};
        _ ->
            {ok, false}
    end.

join(Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Sid),
    case lists:member(Meeting, Record#uce_presence.meetings) of
        true ->
            {ok, updated};
        _ ->
            Meetings = Record#uce_presence.meetings ++ [Meeting],
            ?DB_MODULE:update(Record#uce_presence{meetings=Meetings})
    end.

leave(Sid, Meeting) ->
    {ok, Record} = ?DB_MODULE:get(Sid),
    Meetings = del_entry(Record#uce_presence.meetings, Meeting),
    ?DB_MODULE:update(Record#uce_presence{meetings=Meetings}).

del_entry([Entry], Entry) ->
    [];
del_entry([Entry | Tl], Entry) ->
    Tl;
del_entry([Hd | Tl], Entry) ->
    [Hd] ++ del_entry(Tl, Entry);
del_entry([], _Entry) ->
    []. 
