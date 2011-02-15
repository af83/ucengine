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
-module(utils).

-author('tbomandouki@af83.com').

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([now/0,
         token/0,
         uid/0,

         random/0,
         random/1,
         get/2,
         get/3]).

%% Get current timestamp
now() ->
    {Mega,Sec,Micro} = erlang:now(),
    erlang:round(((Mega*1000000+Sec)*1000000+Micro)/1000).

token() ->
    ?MODULE:random().

uid() ->
    "uid_" ++ ?MODULE:random().

random() ->
    ?MODULE:random(32).
random(0) ->
    [];
random(Length) ->
    [crypto:rand_uniform(48,58)] ++ ?MODULE:random(Length - 1).

get(Params, Key) when is_atom(Key) ->
    [Result] = get(Params, [Key]),
    Result;
get(Params, Keys) ->
    get(Params, Keys, none).
get(Params, Keys, Default) when is_atom(Default) ->
    get(Params, Keys, lists:map(fun(_Elem) ->
                                        Default
                                end,
                                Keys));
get(_Params, [], []) ->
    [];
get(Params, [Key|Keys], [Default|Defaults]) ->
    ValueList = case lists:keysearch(Key, 1, Params) of
                    {value, {Key, Value}} ->
                        [Value];
                    false ->
                        [Default]
                end,
    ValueList ++ ?MODULE:get(Params, Keys, Defaults).

