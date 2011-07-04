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
-module(options_controller).

-export([init/0, options/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='OPTIONS',
                path=['_'],
                callback={?MODULE, options, []}}].

options(Domain, _, [], _) ->
    json_helpers:json(Domain, 200, "",
        [{header, "Access-Control-Allow-Origin: *"},
        {header, "Access-Control-Allow-Methods: GET, POST, PUT, DELETE"},
        {header, "Access-Control-Allow-Headers: X-Requested-With"}]).
