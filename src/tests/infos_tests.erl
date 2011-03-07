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
-module(infos_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

event_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([_, BaseUrl, Testers]) ->
                [?_test(test_get(BaseUrl, Testers)),
                 ?_test(test_update(BaseUrl, Testers)),
                 ?_test(test_update_unauthorized(BaseUrl, Testers))]
        end}.

-include("mongodb.hrl").

test_get(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", {struct, [{"domain", _},
                                   {"metadata", {struct, []}}]}}]} =
        tests_utils:get(BaseUrl, "/infos", Params).

test_update(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "UC Engine"}],
    {struct, [{"result", "ok"}]} = tests_utils:put(BaseUrl, "/infos", Params),
    {struct, [{"result", {struct, [{"domain", _},
                                   {"metadata", {struct, [{"description", "UC Engine"}]}}]}}]} =
        tests_utils:get(BaseUrl, "/infos", Params),

    Params2 = [{"uid", RootUid},
               {"sid", RootSid},
               {"metadata[description]", "UC Engine2"}],
    {struct, [{"result", "ok"}]} = tests_utils:put(BaseUrl, "/infos", Params2),
    {struct, [{"result", {struct, [{"domain", _},
                                   {"metadata", {struct, [{"description", "UC Engine2"}]}}]}}]} =
        tests_utils:get(BaseUrl, "/infos", Params2).

test_update_unauthorized(BaseUrl, [_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid},
              {"metadata[description]", "UC Engine"}],
    {struct, [{"error", "unauthorized"}]} = tests_utils:put(BaseUrl, "/infos", Params).
