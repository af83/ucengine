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
-export([start_link/2,
         add/2,
         all/1,
         get/2,
         get_all/2,
         delete/2,
         assert/3,
         join/3,
         leave/3]).


% gen_server callbacks
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-behaviour(gen_server).

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
                   {ok, NewPid} = uce_vhost_presence_sup:start_child(Domain, [Domain, Uid]),
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

get_all(_Domain, Pid) ->
    gen_server:call(Pid, get_presences).
%
% Return pid of all user connected
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

assert(Domain, Uid, Sid) ->
    case check(Domain, Uid, Sid) of
        {ok, true} ->
            {ok, true};
        {ok, false} ->
            throw({error, unauthorized})
    end.

check(Domain, Uid, Sid) ->
    case get(Domain, Sid) of
        {ok, #uce_presence{user=Uid} = Presence} ->
            {ok, _Presence} = update(Domain, Presence#uce_presence{last_activity=utils:now()}),
            {ok, true};
        {error, not_found} ->
            {ok, false}
    end.

join(Domain, Sid, Meeting) ->
    {ok, Presence} = get(Domain, Sid),
    case lists:member(Meeting, Presence#uce_presence.meetings) of
        true ->
            {ok, updated};
        false ->
            Meetings = [Meeting|Presence#uce_presence.meetings],
            update(Domain, Presence#uce_presence{meetings=Meetings})
    end.

leave(Domain, Sid, Meeting) ->
    {ok, Record} = get(Domain, Sid),
    Meetings = lists:delete(Meeting, Record#uce_presence.meetings),
    update(Domain, Record#uce_presence{meetings=Meetings}).

%
% gen server callbacks
%

-record(state, {
          domain,
          presences = []
         }).

init([Domain, Uid]) ->
    gproc:add_local_name({Domain, uid, Uid}),
    {ok, #state{domain=Domain}}.

handle_call({add_presence, #uce_presence{id=Sid}=Presence}, _From, #state{domain=Domain, presences=Presences} = State) ->
    gproc:add_local_name({Domain, sid, Sid}),
    {reply, ok, State#state{presences=[Presence|Presences]}};

handle_call({get_presence, Sid}, _From, #state{presences=Presences} = State) ->
    {reply, get_presence_by_sid(Sid, Presences), State};

handle_call(get_presences, _From, #state{presences=Presences} = State) ->
    {reply, Presences, State};

handle_call({update_presence, #uce_presence{id=Sid}=NewPresence}, _From, #state{presences=Presences} = State) ->
    {ok, Presence} = get_presence_by_sid(Sid, Presences),
    NewPresences = lists:delete(Presence, Presences),
    {reply, get_presence_by_sid(Sid, Presences), State#state{presences=[NewPresence|NewPresences]}};

handle_call({delete_presence, Sid}, _From, #state{presences=Presences} = State) ->
    {ok, Presence} = get_presence_by_sid(Sid, Presences),
    NewPresences = lists:delete(Presence, Presences),
    case NewPresences of
        [] ->
            {stop, "end of process", {ok, deleted}, State#state{presences=NewPresences}};
        _Other ->
            {reply, {ok, deleted}, State#state{presences=NewPresences}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%
% Private function
%
start_link(Domain, User) ->
    gen_server:start_link(?MODULE, [Domain, User], []).

call_if_proc_found(Domain, Sid, Call) ->
    case gproc:lookup_local_name({Domain, sid, Sid}) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, Call)
    end.

%
% Update presence
%
-spec update(domain(), #uce_presence{}) -> {ok, #uce_presence{}} | {error, not_found}.
update(Domain, #uce_presence{id=Sid} = Presence) ->
    call_if_proc_found(Domain, Sid, {update_presence, Presence}).

-spec get_presence_by_sid(sid(), list(#uce_presence{})) -> {ok, #uce_presence{}} | {error, not_found}.
get_presence_by_sid(_Sid, []) ->
    {error, not_found};
get_presence_by_sid(Sid, [#uce_presence{id=Sid} = Presence|_Presences]) ->
    {ok, Presence};
get_presence_by_sid(Sid, [_Presence|Presences]) ->
    get_presence_by_sid(Sid, Presences).
