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
-module(json_helpers).

-export([unexpected_error/0,
         error/1,
         ok/0,
         true/0,
         false/0,
         created/0,
         created/1,
         json/1]).

format_response(Status, Content) ->
    format_response(Status, "application/json", Content).

format_response(Status, ContentType, Content) ->
    format_response(Status, ContentType, [], Content).

format_response(Status, "application/json" = ContentType, Headers, Content) ->
    Body = mochijson:encode(Content),
    [{status, Status},
     {content, ContentType, lists:flatten(Body)}] ++ Headers;

format_response(Status, ContentType, Headers, Body) ->
    [{status, Status},
     {content, ContentType, lists:flatten(Body)}] ++ Headers.

unexpected_error() ->
    format_response(500, {struct, [{error, unexpected_error}]}).

error(Reason) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Code, {struct, [{error, Reason}]}).

ok() ->
    format_response(200, {struct, [{result, ok}]}).

true() ->
    format_response(200, {struct, [{result, "true"}]}).

false() ->
    format_response(200, {struct, [{result, "false"}]}).

created() ->
    format_response(201, {struct, [{result, created}]}).

created(Id) ->
    format_response(201, {struct, [{result, Id}]}).

json(Content) ->
    format_response(201, {struct, [{result, Content}]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unexpected_error_test() ->
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"unexpected_error\"}"}], unexpected_error()).

error_test() ->
    ?assertMatch([{status, 400}, {content, "application/json", "{\"error\":\"bad_parameters\"}"}], error(bad_parameters)),
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"hello_world\"}"}], error("hello_world")).

format_response_test() ->
    ?assertMatch([{status, 200}, {content, "application/json", "\"{}\""}], format_response(200, "{}")),
    ?assertMatch([{status, 200}, {content, "text/plain", "{}"}], format_response(200, "text/plain", "{}")),
    ?assertMatch([{status, 200}, {content, "text/plain", "{}"}, {header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], format_response(200, "text/plain", [{header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], "{}")).

-endif.
