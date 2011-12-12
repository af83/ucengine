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
-module(uce_middleware_static).

-export([call/2]).

-include("uce.hrl").

static(Req, StaticDir) ->
    handle_static(Req:get(method), Req:resource([lowercase, urldecode]), Req, StaticDir).
handle_static('GET', FilePath, Req, StaticDir) ->
    case misultin_utility:sanitize_path_tokens(FilePath) of
        invalid ->
            stop;
        SanitizedPath ->
            FullFilePath = filename:join([StaticDir, filename:join(SanitizedPath)]),
            case file:read_file_info(FullFilePath) of
                {ok, _FileInfo} ->
                    Req:file(FullFilePath),
                    stop;
                {error, _} ->
                    ok
            end
    end;
handle_static(_, _, _Req, _Static) ->
    ok.

call(#uce_request{arg=Req2} = Request, Response) ->
    case static(Req2, "wwwroot") of
        ok ->
            {ok, Request, Response};
        stop ->
            stop
    end.
