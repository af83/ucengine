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
-include_lib("yaws/include/yaws_api.hrl").

%%
%% Determine the http method
%%
-spec call(Request :: request(), Response :: response()) -> {ok, request(), response()}.
call(#uce_request{qparams=Query,
                  arg=Arg} = Request, Response) ->
    Method = case proplists:lookup("_method", Query) of
                 none ->
                     Arg#arg.req#http_request.method;
                 {"_method", StringMethod} ->
                     list_to_atom(string:to_upper(StringMethod))
             end,
    {ok, Request#uce_request{method=Method}, Response}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

method_override_test() ->
    Arg = #arg{req=#http_request{method='POST'}},
    {ok, Request, _Response} = call(#uce_request{qparams=[{"_method", "put"}], arg=Arg}, #uce_response{}),
    ?assertEqual('PUT', Request#uce_request.method).

method_test() ->
    Arg = #arg{req=#http_request{method='POST'}},
    {ok, Request, _Response} = call(#uce_request{arg=Arg}, #uce_response{}),
    ?assertEqual('POST', Request#uce_request.method).

-endif.
