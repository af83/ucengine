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
-module(url_tests).

-include_lib("eunit/include/eunit.hrl").

url_test_() ->
    {inparallel,
     [?_test(test_bad_url()),
      ?_test(test_options()),
      ?_test(test_head()),
      ?_test(test_get_time()),
      ?_test(test_cors())]}.

test_bad_url() ->
    BaseUrl = fixtures:get_base_url(),
    ?assertMatch({struct, [{"error", "not_found"},
                           {"infos", _Description}]}, tests_utils:get(BaseUrl, "", [])).

test_options() ->
    BaseUrl = fixtures:get_base_url(),
    ?assertMatch({ok, "200", _, _}, tests_utils:options_raw(BaseUrl, "/time")),
    ?assertMatch({ok, "404", _, _}, tests_utils:options_raw(BaseUrl, "/")).

test_head() ->
    BaseUrl = fixtures:get_base_url(),
    ?assertMatch({ok, "200", _, ""}, tests_utils:head_raw(BaseUrl, "/time")),
    ?assertMatch({ok, "404", _, ""}, tests_utils:head_raw(BaseUrl, "/")).

test_get_time() ->
    BaseUrl = fixtures:get_base_url(),
    {struct, [{"result", Time}]} = tests_utils:get(BaseUrl, "/time", []),
    Diff = Time - utils:now(),
    if
        Diff > 1000 ->
            throw({error, too_much_delay, Diff});
        true ->
            nothing
    end.

match_cors([]) ->
    ok;
match_cors([{ok, _, Headers, _}| Requests]) ->
    ?assertMatch("*", proplists:get_value("Access-Control-Allow-Origin", Headers)),
    ?assertMatch("GET, POST, PUT, DELETE", proplists:get_value("Access-Control-Allow-Methods", Headers)),
    ?assertMatch("X-Requested-With", proplists:get_value("Access-Control-Allow-Headers", Headers)),
    match_cors(Requests).

test_cors() ->
    BaseUrl = fixtures:get_base_url(),
    Requests = [
                 tests_utils:get_raw(BaseUrl, "")
               , tests_utils:get_raw(BaseUrl, "/time")
               , tests_utils:options_raw(BaseUrl, "")
               , tests_utils:options_raw(BaseUrl, "/time")
               , tests_utils:head_raw(BaseUrl, "")
               , tests_utils:head_raw(BaseUrl, "/time")
                ],
    match_cors(Requests).
