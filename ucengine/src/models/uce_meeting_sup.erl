%% U.C.Engine - Unified Collaboration Engine
%% Copyright (C) 2011 af83
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Supervises uce_meeting.
%% @see uce_meeting
%%
-module(uce_meeting_sup).

-behaviour(supervisor).

-include("uce.hrl").

% External API
-export([start_link/1, start_child/2, terminate_child/2]).

% Supervisor API
-export([init/1]).

start_link(Domain) ->
    supervisor:start_link({local, uce_vhost_sup:name(Domain, "meeting")}, ?MODULE, []).

init([]) ->
    ets:new(meeting_event_managers, [set, public, named_table]),
    {ok, {{simple_one_for_one, 0, 1},
          [{uce_meeting, {gen_event, start_link, []},
            temporary, brutal_kill, worker, [gen_event]}]}}.

start_child(Domain, MeetingName) ->
    {ok, Pid} = supervisor:start_child(uce_vhost_sup:name(Domain, "meeting"), []),
    ets:insert(meeting_event_managers, {{Domain, MeetingName}, Pid}),
    {ok, Pid}.

terminate_child(Domain, MeetingName) ->
    [[Pid]] = ets:match(meeting_event_managers, {{Domain, MeetingName}, '$1'}),
    true = ets:delete(meeting_event_managers, {Domain, MeetingName}),
    supervisor:terminate_child(uce_vhost_sup:name(Domain, "meeting"), Pid).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

meeting_sup_test() ->
    Domain = "localhost",
    Meeting = "test",

    {ok, _Pid} = uce_meeting_sup:start_link(Domain),
    {ok, Pid} = uce_meeting_sup:start_child(Domain, Meeting),
    ?assert(is_pid(Pid)),
    % self() (this test function) acts as the meeting here; we attach two references
    % so we're simulating two subscribers on the same handler kind (uce_subscription).
    Result = gen_event:add_handler(Pid, {uce_subscription, make_ref()}, [self()]),
    Result = gen_event:add_handler(Pid, {uce_subscription, make_ref()}, [self()]),
    ?assertEqual(ok, Result),
    gen_event:notify(Pid, test_event),

    Result2 = receive
        test_event ->
            true
    after 500 ->
            false
    end,
    ?assert(Result2),

    Result3 = receive
        test_event ->
            true
    after 500 ->
            false
    end,
    ?assert(Result3),

    Pid2 = uce_meeting:get_event_manager(Domain, Meeting),
    ?assertEqual(Pid, Pid2),
    uce_meeting_sup:terminate_child(Domain, Meeting),
    ?assertEqual([], ets:match(meeting_event_managers, {{Domain, Meeting}, Pid})).

-endif.
