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
-module(config).

-author("victor.goya@af83.com").

-include("uce.hrl").

-export([init/1,
         get/1,
         get/2,
         set/2,
         set/3]).

%%
%% Init the config module with path to the config file
%%
init(Path) ->
    case file:consult(Path) of
        {ok, Config} ->
            init2(Config),
            ok;
        {error, Reason} ->
            ?ERROR_MSG("config file error: ~p~n", [Reason]),
            error
    end.

%%
%% Get a global config value
%%
get(Key) ->
    get(global, Key).

%%
%% Get a config value for the specified host
%%
get(Domain, Key) ->
    case ets:lookup(uce_config, {Domain, Key}) of
        [{{Domain, Key}, Value}] ->
            Value;
        _ ->
            undefined
    end.

%%
%% Set a global config value
%%
set(Key, Value) ->
    set(global, Key, Value).

%%
%% Set a config value for the specified host
%%
set(Domain, Key, Value) ->
    ets:insert(uce_config, {{Domain, Key}, Value}).

%
% Private functions
%

init2(Config) ->
    DB = ets:new(uce_config, [set, public, {keypos, 1}, named_table]),
    Hosts = proplists:get_value(hosts, Config),
    lists:foreach(fun({Domain, HostConfig}) ->
                          insert_in(Domain, merge(HostConfig, Config), DB)
                  end, Hosts),
    insert_in(global, Config, DB).

merge(Config, Default) ->
    Config ++ merge_keys(Config, Default).

merge_keys(_Config, []) ->
    [];
merge_keys(Config, [{Key, Value}|R]) ->
    case proplists:lookup(Key, Config) of
        none ->
            [{Key, Value} | merge_keys(Config, R)];
        _ ->
            merge_keys(Config, R)
    end.

insert_in(Domain, Config, DB) ->
    lists:foreach(fun({Key, Value}) ->
                          ets:insert(DB, {{Domain, Key}, Value})
                  end, Config).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

merge_simple_test() ->
    Default = [{bricks, [{"translation", "1"}]}],
    Config = [{bricks, [{"erlyvideo", "2"}]}],
    Result = [{bricks, [{"erlyvideo", "2"}]}],
    ?assertEqual(Result, merge(Config, Default)).

merge_complex_test() ->
    Default = [{bricks, [{"translation", "1"}]}, {admin, []}],
    Config = [{bricks, [{"erlyvideo", "2"}]}],
    Result = [{bricks, [{"erlyvideo", "2"}]}, {admin, []}],
    ?assertEqual(Result, merge(Config, Default)).

config_test() ->
    Configs = [{bricks, [{"erlyvideo", "2"}]},
               {wwwroot, "/var/www"},
               {hosts, [{"localhost", [{bricks, [{"translation", "1"}]}]},
                        {"example.com", [{data, "/var/spool"}]}]}],
    init2(Configs),
    ?assertEqual([{"translation", "1"}], config:get("localhost", bricks)),
    ?assertEqual([{"erlyvideo", "2"}], config:get("example.com", bricks)),
    ?assertEqual("/var/www", config:get(wwwroot)).

-endif.
