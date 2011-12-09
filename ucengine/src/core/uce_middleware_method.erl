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
-module(uce_middleware_method).

-export([call/2]).

-include("uce.hrl").

%%
%% Determine the http method
%%
-spec call(Request :: request(), Response :: response()) -> {ok, request(), response()}.
call(#uce_request{qparams=Query} = Request, Response) ->
    Req2 = case proplists:lookup("_method", Query) of
               none ->
                   Request;
               {"_method", StringMethod} ->
                   Request#uce_request{method=list_to_atom(string:to_upper(StringMethod))}
           end,
    {ok, Req2, Response}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

method_override_test() ->
    {ok, Request, _Response} = call(#uce_request{method='POST', qparams=[{"_method", "put"}]}, #uce_response{}),
    ?assertEqual('PUT', Request#uce_request.method).

method_test() ->
    {ok, Request, _Response} = call(#uce_request{method='POST'}, #uce_response{}),
    ?assertEqual('POST', Request#uce_request.method).

-endif.
