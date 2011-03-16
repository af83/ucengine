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
-module(timeout).

-behaviour(gen_server).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([start_link/0]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_server:cast(?MODULE, run),
    {ok, Pid}.

init([]) ->
    {ok, nothing}.

handle_call(_ , _, State) ->
    {reply, ok, State}.

handle_cast(run, State) ->
    timer:sleep(config:get(timeout_refresh) * 1000),
    {ok, Presences} = get_all_presences(),

    % delete expired presences
    Now = utils:now(),
    lists:foreach(
      fun(#uce_presence{id={_Sid, Domain}=Id, last_activity=LastActivity, timeout=Timeout} = Presence) ->
              if
                  LastActivity + (Timeout * 1000) < Now ->
                      ok = presence_helpers:clean(Domain, Presence),
                      {ok, deleted} = uce_presence:delete(Domain, Id);
                  true ->
                      nothing
              end
      end,
      Presences),
    ?MODULE:handle_cast(run, State),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.

get_all_presences() ->
    {ok, get_all_presences(config:get(hosts))}.

get_all_presences([{Host, _} | TlHost]) ->
    {ok, Presences} = uce_presence:all(Host),
    Presences ++ get_all_presences(TlHost);
get_all_presences([]) -> [].
