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
-module(role_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

user_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([_, BaseUrl, [Root, _Participant, Ugly|_]]) ->
              [?_test(test_add(BaseUrl, Root)),
               ?_test(test_add_unauthorized(BaseUrl, Ugly)),
               ?_test(test_add_conflict(BaseUrl, Root)),
               ?_test(test_add_missing_name(BaseUrl, Root)),
               
               ?_test(test_delete_unauthorized(BaseUrl, Ugly)),
               ?_test(test_delete(BaseUrl, Root)),
               ?_test(test_delete_not_found(BaseUrl, Root)),

               ?_test(test_add_access(BaseUrl, Root)),
               ?_test(test_add_access_not_found(BaseUrl, Root)),
               ?_test(test_add_access_unauthorized(BaseUrl, Ugly)),
               ?_test(test_delete_access(BaseUrl, Root)),
               ?_test(test_delete_access_not_found(BaseUrl, Root)),
               ?_test(test_delete_access_unauthorized(BaseUrl, Ugly))
              ]
      end
    }.

test_add(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"name", "test_role"}],
    {struct, [{"result", "created"}]} =
        tests_utils:post(BaseUrl, "/role/", Params).

test_add_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid},
              {"name", "test_role"}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/role/", Params).

test_add_conflict(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"name", "test_role"}],
    {struct, [{"error", "conflict"}]} =
        tests_utils:post(BaseUrl, "/role/", Params).

test_add_missing_name(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"error", "missing_parameters"}]} =
        tests_utils:post(BaseUrl, "/role/", Params).

test_delete_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/role/test_role", Params).

test_delete(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/role/test_role", Params).

test_delete_not_found(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/role/test_role", Params).

test_add_access(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} =
        tests_utils:post(BaseUrl, "/role/default/acl", Params).

test_add_access_not_found(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/role/unexistent_role/acl", Params).

test_add_access_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/role/default/acl", Params).

test_delete_access(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/role/default/acl", Params).

test_delete_access_not_found(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/role/unexistent_role/acl", Params).

test_delete_access_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid},
              {"object", "testobject"},
              {"action", "testaction"},
              {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/role/default/acl", Params).
