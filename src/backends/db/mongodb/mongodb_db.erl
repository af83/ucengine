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
-module(mongodb_db).

-author('victor.goya@af83.com').

-export([init/2,
         drop/0,
         terminate/0]).

-include("uce.hrl").
-include("mongodb.hrl").

init(Domain, MongoPoolInfos) ->
    catch application:start(emongo),
    [Size, Host, Port, Name] = utils:get_values(MongoPoolInfos,
                                                [{size, "1"},
                                                 {host, "localhost"},
                                                 {port, ?DEFAULT_MONGODB_PORT},
                                                 {database, ?DEFAULT_MONGODB_NAME}]),
    emongo:add_pool(Domain, Host, Port, Name, Size).

drop() ->
    lists:foreach(fun({Domain, _}) ->
                          catch emongo:drop_database(Domain)
                  end,
                  config:get('hosts')).

terminate() ->
    ok.

