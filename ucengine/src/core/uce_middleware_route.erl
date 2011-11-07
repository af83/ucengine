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
-module(uce_middleware_route).

-export([call/2]).

-include("uce.hrl").

%%
%% Call the defined route
%%
call(#uce_request{domain=Domain,
                  route=#uce_route{callback={Module, Function}},
                  match=Match,
                  params=Params} = Request, Response) ->
    ?DEBUG("~p: call ~p:~p matching ~p with ~p~n", [Domain, Module, Function, Match, Params]),
    Now = now(),
    try Module:Function(Domain, Match, Params, Request, Response) of
        Response2 ->
            ?TIMER_APPEND(atom_to_list(Module) ++ "_" ++ atom_to_list(Function), Now),
            {ok, Request, Response2}
    catch
        {error, Reason} ->
            ?ERROR_MSG("~p: error: ~p:~p: ~p ~p~n", [Domain, Module, Function, Reason, Params]),
            {ok, Request, json_helpers:error(Response, Reason)};
        E ->
            ?ERROR_MSG("~p: error: ~p:~p: ~p ~p~n", [Domain, Module, Function, Params, E]),
            {ok, Request, json_helpers:unexpected_error(Response)}
    end.
