%%
%%  U.C.Engine - Unified Collaboration Engine
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
-module(uce_async_stream).

-export([wait/11]).

-include("uce.hrl").

-behaviour(gen_server).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%
% Public API
%

wait(#uce_request{arg=Req2} = Request, #uce_response{headers=Headers}, Domain, Uid, Location, Search, From, Types, Parent, Sid, PreviousEvents) ->
    misultin_req:stream(head, 200, Headers ++ [{"Content-type", "text/event-stream"}], Req2),
    {ok, _Pid} = gen_server:start_link(?MODULE, [Request, Domain, Uid, Location, Search, From, Types, Parent, Sid, PreviousEvents], []),
    ok.

%
% gen_server callbacks
%

init([#uce_request{arg=Req2}, Domain, Uid, Location, Search, From, Types, Parent, Sid, PreviousEvents]) ->
    process_flag(trap_exit, true),
    misultin_req:options([{comet, true}], Req2),
    send_events(Req2, Domain, PreviousEvents),
    uce_meeting:subscribe(self(), Domain, Uid, Location, From, Types, Parent),
    uce_presence:add_stream(Domain, Sid),
    ping(),
    {ok, {Req2,
          Domain,
          Search,
          Sid}}.

handle_call(_ , _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State,_) ->
    {ok, State}.

handle_info({event, Event}, {Req, Domain, Search, _Sid} = State) ->
    case uce_async:filter(Search, Event) of
        false ->
            ok;
        true ->
            send_events(Req, Domain, [Event])
    end,
    {noreply, State};
handle_info(ping, {Req, _Domain, _Search, _Sid} = State) ->
    misultin_req:stream(":\n\n", Req),
    ping(),
    {noreply, State};
handle_info(Event, State) ->
    ?ERROR_MSG("unexpected ~p", [Event]),
    {noreply, State}.

terminate(_Reason, {_, Domain, _, Sid}) ->
    uce_meeting:unsubscribe(self()),
    uce_presence:remove_stream(Domain, Sid),
    ok.

%
% Private API
%

ping() ->
    %% Send a PING command every 15s
    erlang:send_after(15000, self(), ping).

send_events(_, _, []) ->
    ok;
send_events(Req, Domain, [#uce_event{datetime=Datetime} = Event|Events]) ->
    misultin_req:stream("data: ", Req),
    misultin_req:stream(mochijson:encode(json_helpers:to_json(Domain, Event)), Req),
    misultin_req:stream("\nid: ", Req),
    misultin_req:stream(integer_to_list(Datetime), Req),
    misultin_req:stream("\n\n", Req),
    send_events(Req, Domain, Events).
