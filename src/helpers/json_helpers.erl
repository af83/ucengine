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

-compile({no_auto_import,[error/1]}).

-export([unexpected_error/0,
         unexpected_error/1,
         error/1,
         error/2,
         ok/1,
         true/1,
         false/1,
         created/1,
         created/2,
         json/2]).

format_response(Status, Content) ->
    format_response(Status, [], Content).

format_response(Status, Headers, Content) ->
    Body = mochijson:encode(Content),
    [{status, Status},
     {content, "application/json", lists:flatten(Body)}] ++ Headers.

add_cors_headers(Domain) ->
    cors_helpers:format_cors_headers(Domain).

unexpected_error() ->
    format_response(500, {struct, [{error, unexpected_error}]}).

unexpected_error(Domain) ->
    format_response(500, add_cors_headers(Domain), {struct, [{error, unexpected_error}]}).

error(Reason) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Code, {struct, [{error, Reason}]}).

error(Domain, Reason) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Code, add_cors_headers(Domain), {struct, [{error, Reason}]}).

ok(Domain) ->
    format_response(200, add_cors_headers(Domain), {struct, [{result, ok}]}).

true(Domain) ->
    format_response(200, add_cors_headers(Domain), {struct, [{result, "true"}]}).

false(Domain) ->
    format_response(200, add_cors_headers(Domain), {struct, [{result, "false"}]}).

created(Domain) ->
    format_response(201, add_cors_headers(Domain), {struct, [{result, created}]}).

created(Domain, {Id, _}) ->
    format_response(201, add_cors_headers(Domain), {struct, [{result, Id}]});
created(Domain, Id) ->
    format_response(201, add_cors_headers(Domain), {struct, [{result, Id}]}).

json(Domain, Content) ->
    format_response(200, add_cors_headers(Domain), {struct, [{result, Content}]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unexpected_error_test() ->
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"unexpected_error\"}"}], unexpected_error()).

error_test() ->
    ?assertMatch([{status, 400}, {content, "application/json", "{\"error\":\"bad_parameters\"}"}], error(bad_parameters)),
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"hello_world\"}"}], error("hello_world")).

format_response_test() ->
    ?assertMatch([{status, 200}, {content, "application/json", "\"{}\""}], format_response(200, "{}")),
    ?assertMatch([{status, 200}, {content, "application/json", "\"{}\""}, {header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], format_response(200, [{header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], "{}")).

-endif.
