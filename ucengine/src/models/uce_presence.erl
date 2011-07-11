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

% public api
-export([add/2,
         all/1,
         get/2,
         get_all/2,
         delete/2,
         assert/3,
         join/3,
         leave/3]).

-include("uce.hrl").
-include_lib("stdlib/include/qlc.hrl").

%
% Add presence
% Attach a presence to the user
%
-spec add(domain(), #uce_presence{}) -> {ok, sid()}.
add(Domain, #uce_presence{id=none}=Presence) ->
    add(Domain, Presence#uce_presence{id=utils:random()});
add(Domain, #uce_presence{last_activity=0}=Presence) ->
    add(Domain, Presence#uce_presence{last_activity=utils:now()});
add(Domain, #uce_presence{timeout=0}=Presence) ->
    add(Domain, Presence#uce_presence{timeout=config:get(presence_timeout)});
add(Domain, #uce_presence{id=Sid, user=Uid}=Presence) ->
    UPid = case gproc:lookup_local_name({Domain, uid, Uid}) of
               undefined ->
                   {ok, NewPid} = uce_vhost:add_user(Domain, Uid),
                   NewPid;
               Pid ->
                   Pid
           end,
    ok = gen_server:call(UPid, {add_presence, Presence}),
    {ok, Sid}.

%
% Get presence
%
-spec get(domain(), sid()) -> {ok, #uce_presence{}} | {error, not_found}.
get(Domain, Sid) ->
    call_if_proc_found(Domain, Sid, {get_presence, Sid}).

-spec get_all(domain(), pid()) -> list(#uce_presence{}).
get_all(_Domain, Pid) ->
    gen_server:call(Pid, get_presences).
%
% Return pid of all users with at least one session/presence
%
-spec all(domain()) -> {ok, list()}.
all(Domain) ->
    Result = qlc:eval(qlc:q([Pid || {{n, l, {D, Type, _Id}}, Pid, _} <- gproc:table({l, n}), D == Domain, Type == uid])),
    {ok, Result}.

%
% Delete presence
%
-spec delete(domain(), sid()) -> {ok, deleted} | {error, not_found}.
delete(Domain, Sid) ->
    call_if_proc_found(Domain, Sid, {delete_presence, Sid}).

-spec assert(domain(), uid(), sid()) -> {ok, true} | erlang:throw({error, unauthorized}).
assert(Domain, Uid, Sid) ->
    case check(Domain, Uid, Sid) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, unauthorized})
    end.

-spec join(domain(), sid(), meeting()) -> {ok, updated}.
join(Domain, Sid, Meeting) ->
    {ok, Presence} = get(Domain, Sid),
    case lists:member(Meeting, Presence#uce_presence.meetings) of
        true ->
            {ok, updated};
        false ->
            Meetings = [Meeting|Presence#uce_presence.meetings],
            update(Domain, Presence#uce_presence{meetings=Meetings})
    end.

-spec leave(domain(), sid(), meeting()) -> {ok, updated}.
leave(Domain, Sid, Meeting) ->
    {ok, Record} = get(Domain, Sid),
    Meetings = lists:delete(Meeting, Record#uce_presence.meetings),
    update(Domain, Record#uce_presence{meetings=Meetings}).

%
% Private function
%

%
% Update presence
%
-spec update(domain(), #uce_presence{}) -> {ok, #uce_presence{}} | {error, not_found}.
update(Domain, #uce_presence{id=Sid} = Presence) ->
    call_if_proc_found(Domain, Sid, {update_presence, Presence}).

check(Domain, Uid, Sid) ->
    case get(Domain, Sid) of
        {ok, #uce_presence{user=Uid} = Presence} ->
            {ok, _Presence} = update(Domain, Presence#uce_presence{last_activity=utils:now()}),
            {ok, true};
        {error, not_found} ->
            {ok, false}
    end.

call_if_proc_found(Domain, Sid, Call) ->
    case gproc:lookup_local_name({Domain, sid, Sid}) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, Call)
    end.
