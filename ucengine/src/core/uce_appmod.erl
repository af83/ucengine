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
-include_lib("yaws/include/yaws_api.hrl").

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, out(Req), State}.

terminate(_Req, _State) ->
    ok.

call_middlewares(Request, Response) ->
    case call_middlewares(Request, Response, [cors, parse, method, router]) of
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
%% Function called by yaws
%% For each vhost we support, we store to the opaque field the current domain
%%
out(Req) ->
    ?COUNTER('http:request'),
    {Method, Req} = cowboy_http_req:method(Req),
    {AbsPath, Req} = cowboy_http_req:raw_path(Req),
    [{Host, _Config}|_Hosts] = config:get(hosts),
    Path = case AbsPath of
               "/api/"++ ?VERSION ++ P2 ->
                   P2;
               _ ->
                   AbsPath
           end,
    %% TODO: restore vhost
    Request = #uce_request{domain=Host,
                           path=Path,
                           method=Method,
                           arg=Req},

    Response = call_middlewares(Request, #uce_response{}),

    case Response of
        %% normal response
        #uce_response{status=Status, content=Content, headers=Headers} ->
            cowboy_http_req:reply(Status, Headers, Content, Req);
        %% in case of multipart
        Other ->
            Other
    end.
