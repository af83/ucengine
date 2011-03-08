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
         set/1,
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
set(#uce_route{} = Route) ->
    gen_server:cast(?MODULE, {set, Route}).

list() ->
    gen_server:call(?MODULE, list).

init([]) ->
    {ok, ets:new(uce_routes, [bag, public, {keypos, 1}])}.

route(_, _, []) ->
    {error, not_found};
route(Method, Path, [{PathRoute, #uce_route{method=RouteMethod, callback=Handler}}|Routes]) ->
    if
        Method == RouteMethod ->
            case re:run(Path, PathRoute, [{capture, all, list}]) of
                {match, Match} ->
                    [_|Tail] = Match,
                    {ok, Tail, Handler};
                _ ->
                    route(Method, Path, Routes)
            end;
        true ->
            route(Method, Path, Routes)
    end.

handle_call({get, Method, Path}, _From, DB) ->
    {reply, route(Method, Path, ets:tab2list(DB)), DB};
handle_call(list, _From, DB) ->
    Routes = lists:map(fun({_, #uce_route{} = Route}) ->
                               Route
                       end,
                       ets:tab2list(DB)),
    {reply, lists:keysort(1, Routes), DB}.

handle_cast({set, #uce_route{regexp=Regexp} = Route}, DB) ->
    case re:compile("^" ++ Regexp ++ "/?$ ?") of
        {ok, CompiledRegexp} ->
            ets:insert(DB, {CompiledRegexp, Route});
        {error, Reason} ->
            ?ERROR_MSG("Error during route compilation '~p': ~p~n", [Route, Reason])
    end,
    {noreply, DB}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.
