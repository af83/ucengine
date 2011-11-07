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
-module(uce_middleware_router).

-export([call/2]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

%%
%% Match the request to the good route
%%
% Handle options method. If one route match, return ok
call(#uce_request{method='OPTIONS', path=Path}, Response) ->
    case routes:get(Path) of
        {ok, _Route, _Match} ->
            {stop, json_helpers:ok(Response)};
        {error, not_found} ->
            ?ERROR_MSG("options ~p: no route found", [Path]),
            {stop, json_helpers:error(Response, not_found)}
    end;
% Handle head method. If a 'GET' route match, normal process
% Yaws will strip the content
call(#uce_request{method='HEAD'} = Request, Response) ->
    call(Request#uce_request{method='GET'}, Response);
call(#uce_request{method=Method, path=Path, arg=Arg} = Request, Response) ->
    ContentType = Arg#arg.headers#headers.content_type,
    case routes:get(Method, Path, ContentType) of
        {ok, Route, Match} ->
            {ok, Request#uce_request{route=Route, match=Match}, Response};
        {error, not_found} ->
            Description = lists:flatten(io_lib:format("~s on ~s did not match any route.", [Method, Path])),
            ?ERROR_MSG(Description, []),
            {stop, json_helpers:error(Response, not_found, Description)}
    end.
