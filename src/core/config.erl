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
-module(config).

-author("victor.goya@af83.com").

-behaviour(gen_server).

-export([start_link/1,
         get/1,
         get/2]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

start_link(Path) ->
    case file:consult(Path) of
        {ok, Configs} ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [Configs], []);
        {error, Reason} ->
            {error, Reason}
    end.

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

get(Key) ->
    ?MODULE:get(global, Key).

get(Domain, Key) ->
    gen_server:call(?MODULE, {get, Domain, Key}).

init([UCEConfig]) ->
    DB = ets:new(uce_config, [duplicate_bag, private, {keypos, 1}]),
    Hosts = proplists:get_value(hosts, UCEConfig),
    lists:foreach(fun({Domain, HostConfig}) ->
                          insert_in(Domain, merge(HostConfig, UCEConfig), DB)
                  end, Hosts),
    insert_in(global, UCEConfig, DB),
    {ok, DB}.

insert_in(Domain, Config, DB) ->
    lists:foreach(fun({Key, Value}) ->
                          ets:insert(DB, {Key, Value, Domain})
                  end, Config).

handle_call({get, Domain, Key}, _From, DB) ->
    Reply = case ets:match_object(DB, {Key, '_', Domain}) of
                [{Key, Value, Domain}] ->
                    Value;
                _ ->
                    undefined
            end,
    {reply, Reply, DB}.

handle_cast({set, Domain, {Key, Value}}, DB) ->
    ets:insert(DB, {Key, Value, Domain}),
    {noreply, DB}.

handle_info(_Info, State) ->
    {reply, State}.

code_change(_,State,_) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

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
               {root, "/var/www"},
               {hosts, [{"localhost", [{bricks, [{"translation", "1"}]}]},
                        {"example.com", [{data, "/var/spool"}]}]}],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Configs], []),
    ?assertEqual([{"translation", "1"}], config:get("localhost", bricks)),
    ?assertEqual([{"erlyvideo", "2"}], config:get("example.com", bricks)),
    ?assertEqual("/var/www", config:get(root)).

-endif.
