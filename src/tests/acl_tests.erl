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
-module(acl_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

acl_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([_, BaseUrl, Testers]) ->
	      [?_test(test_add(BaseUrl, Testers)),
	       ?_test(test_add_not_found_user(BaseUrl, Testers)),
	       ?_test(test_add_not_found_meeting(BaseUrl, Testers)),
	       ?_test(test_add_unauthorized(BaseUrl, Testers)),

	       ?_test(test_check_false_location(BaseUrl, Testers)),
	       ?_test(test_check_false_location_without_meeting(BaseUrl, Testers)),
	       ?_test(test_check_false_conditions(BaseUrl, Testers)),

	       ?_test(test_check_unauthorized(BaseUrl, Testers)),

	       ?_test(test_check_true(BaseUrl, Testers)),
	       ?_test(test_check_true_without_meeting(BaseUrl, Testers)),

	       ?_test(test_delete(BaseUrl, Testers)),
	       ?_test(test_delete_unauthorized(BaseUrl, Testers)),
	       ?_test(test_delete_not_found_meeting(BaseUrl, Testers)),
	       ?_test(test_delete_not_found_user(BaseUrl, Testers))]
      end
    }.

test_add(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", Params).

test_add_not_found_user(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put(BaseUrl, "/user/unexistentuser/acl/testobject/testaction/testmeeting", Params).

test_add_not_found_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/testobject/testaction/unexistentmeeting", Params).

test_add_unauthorized(BaseUrl, [_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:put(BaseUrl, "/user/unexistentuser/acl/testobject/testaction/testmeeting", Params).

test_check_true(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "true"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", Params).

test_check_unauthorized(BaseUrl, [_, {UglyUid, UglySid}]) ->
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", Params).

test_check_true_without_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/testobject/testaction/", Params),
    {struct, [{"result", "true"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/testobject/testaction/", Params).

test_check_false_location(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/othermeeting", Params).

test_check_false_location_without_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/", Params).

test_check_false_conditions(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "c"}],
    {struct, [{"result", "false"}]} = 
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", Params).

test_delete(BaseUrl, [{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"result", "ok"}]} =
	tests_utils:delete(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", Params),
    ParamsCheck = [{"uid", RootUid},
		   {"sid", RootSid},
		   {"conditions[a]", "b"}],
    {struct, [{"result", "false"}]} =
	tests_utils:get(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", ParamsCheck).

test_delete_unauthorized(BaseUrl, [{RootUid, RootSid}, {UglyUid, UglySid}]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", ParamsAdd),
    Params = [{"uid", UglyUid},
	      {"sid", UglySid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:delete(BaseUrl, "/user/participant.user@af83.com/acl/event/get/unexistentmeeting", Params).
    
test_delete_not_found_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete(BaseUrl, "/user/participant.user@af83.com/acl/event/get/unexistentmeeting", Params).

test_delete_not_found_user(BaseUrl, [{RootUid, RootSid}, _]) ->
    ParamsAdd = [{"uid", RootUid},
		 {"sid", RootSid},
		 {"conditions[a]", "b"}],
    {struct, [{"result", "created"}]} = 
	tests_utils:put(BaseUrl, "/user/participant.user@af83.com/acl/event/get/testmeeting", ParamsAdd),
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"conditions[a]", "b"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete(BaseUrl, "/user/unexistentuser/acl/event/get/testmeeting", Params).
