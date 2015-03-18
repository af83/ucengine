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
-module(uce_yaws).

-include("uce.hrl").
-include_lib("yaws/include/yaws.hrl").

-export([child_spec/0]).

-import(lists, [member/2]).

child_spec() ->
    [{DefaultHost, _Config}|_Hosts] = config:get(hosts),
    {ok, _SCList, _GC, ChildSpecs} = yaws_api:embedded_start_conf(config:get(DefaultHost, wwwroot),
                        [{servername, DefaultHost},
                         {listen, config:get(bind_ip)},
                         {port, config:get(port)},
                         {access_log, true},
                         {partial_post_size, nolimit},
                         {opaque, DefaultHost},
                         {appmods, [{"/api/" ++ ?VERSION, uce_appmod}]}],
                        [{flags, [{auth_log, false},
                                  {copy_errlog, false},
                                  {debug, false}
                                 ]},
                         {logdir, config:get(log_dir)},
                         {cache_refresh_secs, config:get(cache_refresh)}]),
    ChildSpecs.
