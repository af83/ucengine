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
-module(http_helpers).

-export([error_to_code/1]).

error_to_code(Error) ->
    case Error of
        % 400 Bad Request
        bad_parameters -> 400;
        missing_parameters -> 400;
        % 401 Unauthorized
        unauthorized -> 401;
        % 403 Forbidden
        bad_credentials -> 403;
        % 404 Not Found
        not_found -> 404;
        % 408 Request Timeout
        request_timeout -> 408;
        % 409 Conflict
        conflict -> 409;
        % 500 Internal Server Error
        unexpected_error -> 500;
        % 501 Not Implemented
        not_implemented -> 501;
        % Default error
        _ -> 500
    end.
