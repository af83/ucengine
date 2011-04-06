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
-module(routes).

-behaviour(gen_server).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([start_link/0,
         get/2,
         list/0]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Method, Path) ->
    gen_server:call(?MODULE, {get, Method, Path}).

list() ->
    gen_server:call(?MODULE, list).

init([]) ->
    TableId = ets:new(uce_routes, [bag, public, {keypos, 1}]),
    setup_routes(TableId),
    {ok, TableId}.

handle_call({get, Method, Path}, _From, DB) ->
    {reply, route(Method, Path, ets:tab2list(DB)), DB};
handle_call(list, _From, DB) ->
    Routes = lists:map(fun({_, #uce_route{} = Route}) ->
                               Route
                       end,
                       ets:tab2list(DB)),
    {reply, lists:keysort(1, Routes), DB}.

handle_cast(_Request, DB) ->
    {noreply, DB}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.

route(_, _, []) ->
    {error, not_found};
route(Method, Path, [{PathRoute, #uce_route{method=Method, callback=Handler}}|Routes]) ->
    case re:run(Path, PathRoute, [{capture, all, list}]) of
        {match, Match} ->
            [_|Tail] = Match,
            {ok, Tail, Handler};
        _ ->
            route(Method, Path, Routes)
    end;
route(Method, Path, [{_PathRoute, #uce_route{method=_RouteMethod, callback=_Handler}}|Routes]) ->
    route(Method, Path, Routes).

setup_route(Controller, DB) ->
    [set(Route, DB) || Route <- Controller:init()].

setup_routes(DB) ->
    % TODO : make list of controllers more generic
    [ setup_route(Controller, DB) || Controller <- [user_controller,
                                                    presence_controller,
                                                    meeting_controller,
                                                    role_controller,
                                                    event_controller,
                                                    file_controller,
                                                    time_controller,
                                                    infos_controller,
                                                    search_controller]].

set(#uce_route{regexp=Regexp} = Route, DB) ->
    {ok, CompiledRegexp} = re:compile("^" ++ Regexp ++ "/?$ ?"),
    true = ets:insert(DB, {CompiledRegexp, Route}).
