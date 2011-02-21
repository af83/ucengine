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

-export([init/1,
         drop/0,
         terminate/0]).

-include("uce.hrl").
-include("mongodb.hrl").

init({Pool, MongoPoolInfos}) ->
    case utils:get(MongoPoolInfos,
                   [size, host, port, database],
                   [1, "localhost", ?DEFAULT_MONGODB_PORT, ?DEFAULT_MONGODB_NAME]) of
        [Size, Host, Port, Name] ->
            application:start(emongo),
            emongo:add_pool(Pool, Host, Port, Name, Size),
            ok;
        _ ->
            {error, bad_configuration}
    end.

drop() ->
    emongo:drop_database(?MONGO_POOL).

terminate() ->
    ok.
