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
         json/1,
         xml/1]).

unexpected_error() ->
    Content = mochijson:encode({struct, [{error, unexpected_error}]}),
    [{status, 500},
     {content, "application/json", lists:flatten(Content)}].

error(Reason) ->
    case http_helpers:error_to_code(Reason) of
	500 ->
	    ?MODULE:unexpected_error();
	Code ->
	    Content = mochijson:encode({struct, [{error, Reason}]}),
	    [{status, Code},
	     {content, "application/json", lists:flatten(Content)}]
    end.

ok() ->
    Content = mochijson:encode({struct, [{result, ok}]}),
    [{status, 200},
     {content, "application/json", lists:flatten(Content)}].

true() ->
    Content = mochijson:encode({struct, [{result, "true"}]}),
    [{status, 200},
     {content, "application/json", lists:flatten(Content)}].

false() ->
    Content = mochijson:encode({struct, [{result, "false"}]}),
    [{status, 200},
     {content, "application/json", lists:flatten(Content)}].

created() ->
    Content = mochijson:encode({struct, [{result, created}]}),
    [{status, 201},
     {content, "application/json", lists:flatten(Content)}].

created(Id) ->
    Content = mochijson:encode({struct, [{result, Id}]}),
    [{status, 201},
     {content, "application/json", lists:flatten(Content)}].

xml(Content) ->
    [{status, 200},
     {content, "application/xml", lists:flatten(Content)}].

json(Content) ->
    JSONContent = mochijson:encode({struct, [{result, Content}]}),
    [{status, 200},
     {content, "application/json", lists:flatten(JSONContent)}].
