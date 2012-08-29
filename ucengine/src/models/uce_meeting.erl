%%
%%  U.C.Engine - Unified Collaboration Engine
%%  Copyright (C) 2012 Stormz
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
-module(uce_meeting).

% public api
-export([add/2,
         delete/2,
         update/2,
         get/2,
         list/1,
         join/3,
         leave/3,
         roster/2,
         exists/2,
         publish/2,
         subscribe/7,
         unsubscribe/1]).

% gen_server callbacks
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-behaviour(gen_server).

-include("uce.hrl").

%
% Public API callbacks
%

-spec add(domain(), meeting()) -> {ok, created} | erlang:throw({error, conflict}).
add(Domain, #uce_meeting{id=Id} = Meeting) ->
    case exists(Domain, Id) of
        true ->
            throw({error, conflict});
        false ->
            (db:get(?MODULE, Domain)):add(Domain, Meeting)
    end.

-spec delete(domain(), meeting_id()) -> {ok, deleted} | erlang:throw({error, not_found}).
delete(Domain, Id) ->
    {ok, Pid} = get_or_start(Domain, Id),
    ok = gen_server:call(Pid, delete),
    {ok, deleted}.

-spec get(domain(), meeting_id()) -> {ok, meeting()} | erlang:throw({error, not_found}).
get(Domain, Id) ->
    {ok, Pid} = get_or_start(Domain, Id),
    gen_server:call(Pid, get).

-spec update(domain(), meeting()) -> {ok, updated} | erlang:throw({error, not_found}).
update(Domain, #uce_meeting{id=Id} = Meeting) ->
    {ok, Pid} = get_or_start(Domain, Id),
    gen_server:call(Pid, {update, Meeting}).

-spec list(domain()) -> {ok, list(meeting)} | erlang:throw({error, bad_parameters}).
list(Domain) ->
    (db:get(?MODULE, Domain)):list(Domain).

-spec exists(domain(), meeting_id()) -> boolean().
exists(_Domain, "") ->
    true; % root
exists(Domain, Id) ->
    case catch get_or_start(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            true
    end.

-spec join(domain(), meeting_id(), user_id()) -> {ok, updated} | erlang:throw({error, not_found}).
join(Domain, Id, User) ->
    {ok, Pid} = get_or_start(Domain, Id),
    ok = gen_server:call(Pid, {join, User}),
    {ok, updated}.

-spec leave(domain(), meeting_id(), user_id()) -> {ok, updated} | erlang:throw({error, not_found}).
leave(Domain, Id, User) ->
    {ok, Pid} = get_or_start(Domain, Id),
    case gen_server:call(Pid, {leave, User}) of
        ok ->
            {ok, updated};
        not_found ->
            throw({error, not_found})
    end.

-spec roster(domain(), meeting_id()) -> {ok, list(user())} | erlang:throw({error, not_found}).
roster(Domain, Id) ->
    {ok, Pid} = get_or_start(Domain, Id),
    gen_server:call(Pid, get_roster).

publish(Domain, Event) ->
    ?PUBSUB_MODULE:publish(Domain, Event).

subscribe(Pid, Domain, Uid, Location, From, Types, Parent) ->
    ?PUBSUB_MODULE:subscribe(Pid, Domain, Uid, Location, From, Types, Parent).

unsubscribe(Pid) ->
    ?PUBSUB_MODULE:unsubscribe(Pid).


%
% gen server callbacks
%

-record(state, {
          domain,
          meeting
         }).

init([Domain, #uce_meeting{id=Id} = Meeting]) ->
    gproc:add_local_name({Domain, meeting, Id}),
    {ok, #state{domain=Domain, meeting=Meeting}}.

handle_call(get, _From, #state{meeting=Meeting} = State) ->
    {reply, {ok, Meeting}, State};
handle_call({update, Meeting}, _From, #state{domain=Domain} = State) ->
    Result = (db:get(?MODULE, Domain)):update(Domain, Meeting),
    {reply, Result, State#state{meeting=Meeting}};
handle_call({join, User}, _From, #state{domain=Domain, meeting=Meeting} = State) ->
    case lists:member(User, Meeting#uce_meeting.roster) of
        false ->
            NewMeeting = Meeting#uce_meeting{roster=Meeting#uce_meeting.roster ++ [User]},
            {ok, updated} = (db:get(?MODULE, Domain)):update(Domain, NewMeeting),
            {reply, ok, State#state{meeting=NewMeeting}};
        true ->
            {reply, ok, State}
    end;
handle_call({leave, User}, _From, #state{domain=Domain, meeting=Meeting} = State) ->
    case lists:member(User, Meeting#uce_meeting.roster) of
        false ->
            {reply, not_found, State};
        true ->
            Roster = lists:subtract(Meeting#uce_meeting.roster, [User]),
            NewMeeting = Meeting#uce_meeting{roster=Roster},
            {ok, updated} = (db:get(?MODULE, Domain)):update(Domain, NewMeeting),
            {reply, ok, State#state{meeting=NewMeeting}}
    end;
handle_call(get_roster, _From, #state{meeting=Meeting} = State) ->
    {reply, {ok, Meeting#uce_meeting.roster}, State};
handle_call(delete, _From, #state{domain=Domain, meeting=#uce_meeting{id=Id}} = State) ->
    {ok, deleted} = (db:get(?MODULE, Domain)):delete(Domain, Id),
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private function
%

start_link(Domain, Meeting) ->
    gen_server:start_link(?MODULE, [Domain, Meeting], []).

get_pid_of(Domain, Id) ->
    case gproc:lookup_local_name({Domain, meeting, Id}) of
        undefined ->
            {error, not_found};
        Pid ->
            {ok, Pid}
    end.

get_or_start(Domain, Id) ->
    case get_pid_of(Domain, Id) of
        {error, not_found} ->
            case catch (db:get(?MODULE, Domain)):get(Domain, Id) of
                {error, Reason} ->
                    throw({error, Reason});
                {ok, Meeting}->
                    start_link(Domain, Meeting)
            end;
        {ok, Pid} ->
            {ok, Pid}
    end.
