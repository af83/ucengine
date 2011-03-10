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

-export([add/2,
         all/1,
         get/2,
         delete/2,
         update/2,
         exists/2,
         check/3,
         assert/3,
         join/3,
         leave/3]).

-include("uce.hrl").

add(Domain, #uce_presence{id=none}=Presence) ->
    add(Domain, Presence#uce_presence{id=utils:random()});
add(Domain, #uce_presence{last_activity=0}=Presence) ->
    add(Domain, Presence#uce_presence{last_activity=utils:now()});
add(Domain, #uce_presence{timeout=0}=Presence) ->
    add(Domain, Presence#uce_presence{timeout=config:get(presence_timeout)});
add(Domain, #uce_presence{}=Presence) ->
    apply(db:get(?MODULE, Domain), add, [Presence]).

get(Domain, Id) ->
    apply(db:get(?MODULE, Domain), get, [Domain, Id]).

all(Domain) ->
    apply(db:get(?MODULE, Domain), all, [Domain]).

delete(Domain, Id) ->
    case ?MODULE:exists(Domain, Id) of
        false ->
            throw({error, not_found});
        true ->
            apply(db:get(?MODULE, Domain), delete, [Domain, Id])
    end.

update(Domain, #uce_presence{}=Presence) ->
    case ?MODULE:exists(Domain, Presence#uce_presence.id) of
        false ->
            throw({error, not_found});
        true ->
            apply(db:get(?MODULE, Domain), update, [Presence])
    end.

exists(Domain, Id) ->
    case catch ?MODULE:get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            true
    end.

assert(Domain, User, Sid) ->
    case check(Domain, User, Sid) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, unauthorized})
    end.

check(Domain, User, Sid) ->
    {ok, Record} = uce_presence:get(Domain, Sid),
    case Record#uce_presence.user of
        User ->
            uce_presence:update(Domain, Record#uce_presence{last_activity=utils:now()}),
            {ok, true};
        _ ->
            {ok, false}
    end.

join(Domain, Sid, Meeting) ->
    {ok, Record} = apply(db:get(?MODULE, Domain), get, [Domain, Sid]),
    case lists:member(Meeting, Record#uce_presence.meetings) of
        true ->
            {ok, updated};
        _ ->
            Meetings = Record#uce_presence.meetings ++ [Meeting],
            apply(db:get(?MODULE, Domain), update, [Record#uce_presence{meetings=Meetings}])
    end.

leave(Domain, Sid, Meeting) ->
    {ok, Record} = apply(db:get(?MODULE, Domain), get, [Domain, Sid]),
    Meetings = del_entry(Record#uce_presence.meetings, Meeting),
    apply(db:get(?MODULE, Domain), update, [Record#uce_presence{meetings=Meetings}]).

del_entry([Entry], Entry) ->
    [];
del_entry([Entry | Tl], Entry) ->
    Tl;
del_entry([Hd | Tl], Entry) ->
    [Hd] ++ del_entry(Tl, Entry);
del_entry([], _Entry) ->
    [].
