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
-module(config).

-author("victor.goya@af83.com").

-behaviour(gen_server).

-export([start_link/1,
         set/2,
         set/3,
         get/1,
         get/2]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

start_link(Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    case file:consult(Path) of
        {ok, Configs} ->
            lists:foreach(fun({Key, Value}) ->
                                  config:set(Key, Value)
                          end,
                          Configs),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

get(Key) ->
    ?MODULE:get(global, Key).
set(Key, Value) ->
    ?MODULE:set(global, Key, Value).

get(Domain, Key) ->
    gen_server:call(?MODULE, {get, Domain, Key}).
set(Domain, Key, Value) ->
    gen_server:cast(?MODULE, {set, Domain, {Key, Value}}).

init([]) ->
    {ok, {ets:new(uce_config, [set, public, {keypos, 1}])}}.

handle_call({get, Domain, Key}, _From, {DB}) ->
    Reply = case ets:lookup(DB, Key) of
                [{Key, Value, Domain}] ->
                    Value;
                _ ->
                    undefined
            end,
    {reply, Reply, {DB}}.

handle_cast({set, Domain, {Key, Value}}, {DB}) ->
    ets:insert(DB, {Key, Value, Domain}),
    {noreply, {DB}}.

handle_info(_Info, State) ->
    {reply, State}.

code_change(_,State,_) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
