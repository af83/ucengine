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
-module(uce_middleware_cors).

-export([call/2]).

-include("uce.hrl").

%%
%% Add cors headers on response
%%
-spec call(Request :: request(), Response :: response()) -> {ok, request(), response()}.
call(Request, #uce_response{headers=Headers} = Response) ->
    CorsHeaders = [{"Access-Control-Allow-Origin", "*"},
                   {"Access-Control-Allow-Methods", "GET, POST, PUT, DELETE"},
                   {"Access-Control-Allow-Headers", "X-Requested-With"}],
    {ok, Request, Response#uce_response{headers=Headers ++ CorsHeaders}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cors_test() ->
    {ok, _Request, #uce_response{headers=Headers}} = call(#uce_request{}, #uce_response{}),
    ?assertMatch([{"Access-Control-Allow-Origin", "*"},
                  {"Access-Control-Allow-Methods", "GET, POST, PUT, DELETE"},
                  {"Access-Control-Allow-Headers", "X-Requested-With"}], Headers).

-endif.
