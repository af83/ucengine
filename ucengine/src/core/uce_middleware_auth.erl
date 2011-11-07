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
-module(uce_middleware_auth).

-export([call/2]).

-include("uce.hrl").

%%
%% Extract sid and uid parameters from query and check the session
%%
-spec call(Request :: request(), Response :: response()) -> {ok, request(), response()} | {stop, response()}.
call(#uce_request{domain=Domain, qparams=Query, params=Params} = Request, Response) ->
    Uid = proplists:get_value("uid", Query),
    Sid = proplists:get_value("sid", Query),
    case uce_presence:assert(Domain, Uid, Sid) of
        ok ->
            {ok, Request#uce_request{params=[Uid, Sid] ++ Params}, Response};
        unauthorized ->
            {stop, json_helpers:error(Response, unauthorized)}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

auth_test() ->
    application:start(gproc),
    ?assertMatch({stop, #uce_response{status=401}},
                 call(#uce_request{qparams=[{"sid", "a"},
                                            {"uid", "a"}]}, #uce_response{})),
    ?assertMatch({stop, #uce_response{status=401}},
                 call(#uce_request{qparams=[]}, #uce_response{})).

-endif.
