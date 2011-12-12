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
-module(uce_appmod).

-include("uce.hrl").

-export([out/1]).

call_middlewares(Request, Response) ->
    case call_middlewares(Request, Response, [cors, static, parse, method, router]) of
        {stop, Resp} ->
            Resp;
        {ok, Req, Resp} ->
            case call_middlewares(Req, Resp, Req#uce_request.route#uce_route.middlewares ++ [route]) of
                {stop, Resp2} ->
                    Resp2;
                {ok, _Req2, Resp2} ->
                    Resp2
            end;
        Other ->
            Other
    end.

call_middlewares(Request, Response, []) ->
    {ok, Request, Response};
call_middlewares(Request, Response, [Middleware|Middlewares]) ->
    case call_middleware(Request, Response, Middleware) of
        {ok, Req2, Response2} ->
            call_middlewares(Req2, Response2, Middlewares);
        {stop, Response2} ->
            {stop, Response2};
        Other ->
            Other
    end.

middleware_name(Middleware) ->
    list_to_atom(lists:concat([uce_middleware_, Middleware])).

call_middleware(Request, Response, {Middleware, Params}) ->
    (middleware_name(Middleware)):call(Request, Response, Params);
call_middleware(Request, Response, Middleware) ->
    (middleware_name(Middleware)):call(Request, Response).

%%
%% Function called by Misultin
%% TODO: restore vhost support
%%
out(Req) ->
    ?COUNTER('http:request'),
    [{Host, _Config}|_Hosts] = config:get(hosts),
    {abs_path, AbsPath} = misultin_req:get(uri, Req),
    Path = case AbsPath of
               "/api/"++ ?VERSION ++ P2 ->
                   P2;
               _ ->
                   AbsPath
           end,

    Request = #uce_request{domain=Host,
                           method=misultin_req:get(method, Req),
                           path=Path,
                           arg=Req},

    Response = call_middlewares(Request, #uce_response{}),

    case Response of
        %% normal response
        #uce_response{status=Status, content=Content, headers=Headers} ->
            misultin_req:respond(Status, Headers, Content, Req);
        %% in case of multipart
        Other ->
            Other
    end.
