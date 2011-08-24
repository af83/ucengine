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

-export([wait/8]).

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

wait(Domain, Location, Search, From, Types, Parent, Sid, PreviousEvents) ->
    YawsPid = self(),
    {ok, _Pid} = gen_server:start_link(?MODULE, [YawsPid, Domain, Location, Search, From, Types, Parent, Sid, PreviousEvents], []),
    {streamcontent_with_timeout, "text/event-stream", <<>>, infinity}.

%
% gen_server callbacks
%

init([YawsPid, Domain, Location, Search, _From, _Types, _Parent, Sid, PreviousEvents]) ->
    process_flag(trap_exit, true),
    link(YawsPid),
    send_events(YawsPid, Domain, PreviousEvents),
    uce_meeting:subscribe(Domain, Location, self()),
    uce_presence:add_stream(Domain, Sid),
    {ok, {YawsPid,
          Domain,
          Search,
          Sid}}.

handle_call(_ , _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State,_) ->
    {ok, State}.

handle_info({event, Event}, {YawsPid, Domain, Search, _Sid} = State) ->
    case uce_async:filter(Search, Event) of
        false ->
            ok;
        true ->
            send_events(YawsPid, Domain, [Event])
    end,
    {noreply, State};
handle_info(Event, State) ->
    ?ERROR_MSG("unexpected ~p", [Event]),
    {noreply, State}.

terminate(_Reason, {_, Domain, _, Sid}) ->
    ?PUBSUB_MODULE:unsubscribe(self()),
    uce_presence:remove_stream(Domain, Sid),
    ok.

%
% Private API
%

send_events(_, _, []) ->
    ok;
send_events(YawsPid, Domain, [#uce_event{datetime=Datetime} = Event|Events]) ->
    yaws_api:stream_chunk_deliver(YawsPid, "data: "),
    yaws_api:stream_chunk_deliver(YawsPid, mochijson:encode(json_helpers:to_json(Domain, Event))),
    yaws_api:stream_chunk_deliver(YawsPid, "\nid: "),
    yaws_api:stream_chunk_deliver(YawsPid, integer_to_list(Datetime)),
    yaws_api:stream_chunk_deliver(YawsPid, "\n\n"),
    send_events(YawsPid, Domain, Events).
