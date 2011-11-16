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
-module(meeting_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

meeting_test_() ->
    { setup
      , fun fixtures:setup/0
      , fun fixtures:teardown/1
      , fun([_, BaseUrl, [Root, _Participant, Ugly|_]]) ->
                [ ?_test(test_create(BaseUrl, Root)),
                  ?_test(test_create_conflict(BaseUrl, Root)),
                  ?_test(test_create_unauthorized(BaseUrl, Ugly)),

                  ?_test(test_get(BaseUrl, Root)),
                  ?_test(test_get_not_found_meeting(BaseUrl, Root)),

                  ?_test(test_list(BaseUrl, Root)),

                  ?_test(test_update(BaseUrl, Root)),
                  ?_test(test_update_not_found_meeting(BaseUrl, Root)),
                  ?_test(test_update_unauthorized(BaseUrl, Ugly)),

                  ?_test(test_join(BaseUrl, Root)),
                  ?_test(test_join_not_found_meeting(BaseUrl, Root)),
                  ?_test(test_join_not_found_uid(BaseUrl)),
                  ?_test(test_join_unauthorized(BaseUrl, Ugly)),

                  ?_test(test_leave(BaseUrl, Root)),
                  ?_test(test_leave_not_found_meeting(BaseUrl, Root)),
                  ?_test(test_leave_not_found_uid(BaseUrl, Root)),
                  ?_test(test_leave_unauthorized(BaseUrl, Ugly))]
        end
    }.

test_create(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}
               , {"name", "newmeeting"}
               , {"metadata[description]", "Meeting"}],
    {struct, [{"result", "created"}]} = tests_utils:post(BaseUrl, "/meeting/", Params).

test_create_conflict(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
             , {"name", "newmeeting"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "conflict"}]} = tests_utils:post(BaseUrl, "/meeting/", Params).

test_create_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid}
             , {"sid", UglySid}
             , {"name", "newmeeting2"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/meeting/", Params).


test_get(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}],
    {struct, [{"result",
	       {struct,
		[{"name", "newmeeting"},
                 {"domain", _},
		 {"metadata",{struct, [{"description", "Meeting"}]}}]}}]} =
	tests_utils:get(BaseUrl, "/meeting/newmeeting", Params).

test_get_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:get(BaseUrl, "/meeting/unexistentmeeting", Params).

test_list(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    JSON = tests_utils:get(BaseUrl, "/meeting", Params),
    test_meeting_in_list(["testmeeting"], JSON),
    test_meeting_in_list(["meeting2"], JSON),
    test_meeting_in_list(["meeting3"], JSON).

test_update(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}
               , {"metadata[description]", "A new description"}],
    {struct, [{"result", "ok"}]} =
        tests_utils:put(BaseUrl, "/meeting/testmeeting", Params),
    {struct, [{"result",
	       {struct,
		[{"name", "testmeeting"},
         {"domain", _},
		 {"metadata",{struct, [{"description", "A new description"}]}}]}}]} =
	tests_utils:get(BaseUrl, "/meeting/testmeeting", Params).

test_update_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put(BaseUrl, "/meeting/unexistentmeeting", Params).

test_update_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid}
             , {"sid", UglySid}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:put(BaseUrl, "/meeting/testmeeting", Params).


test_join(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}],

    {struct, [{"result", "ok"}]} =
	tests_utils:post(BaseUrl, "/meeting/testmeeting/roster/", Params),

    {struct, [{"result", {array, Array}}]} =
        tests_utils:get(BaseUrl, "/meeting/testmeeting/roster/", Params),
    [{struct,[{"uid",RootUid},
              {"name",_},
              {"domain",_},
              {"auth","password"},
              {"metadata",{struct,[]}}]}] = Array.

test_join_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
	       , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/meeting/unexistentmeeting/roster/", Params).

test_join_not_found_uid(BaseUrl) ->
    Params = [ {"uid", "unexistentuid"},
               {"sid", ""}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/meeting/testmeeting/roster/", Params).

test_join_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/meeting/testmeeting/roster/", Params).

test_leave(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/meeting/testmeeting/roster/" ++ RootUid, Params),
    {struct, [{"result", {array, Array}}]} =
        tests_utils:get(BaseUrl, "/meeting/testmeeting/roster", Params),
    [] = Array.

test_leave_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/meeting/unexistentmeeting/roster/" ++ RootUid, Params).

test_leave_not_found_uid(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/meeting/testmeeting/roster/unexistentuid", Params).

test_leave_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid},
               {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/meeting/testmeeting/roster/test.user@af83.com", Params).

test_meeting_in_list(Id, {struct, [{"result", {array, List}}]}) ->
    test_meeting_in_list(Id, List);
test_meeting_in_list(Id, []) ->
    throw({not_found, Id});
test_meeting_in_list(Id, [Meeting|Tail]) ->
    {struct,
     [{"name", MeetingName},
      {"domain", _},
      {"metadata",_}]} = Meeting,
    case Id of
	[MeetingName] ->
	    true;
	_ ->
	    test_meeting_in_list(Id, Tail)
    end.
