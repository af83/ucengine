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
		  ?_test(test_create_bad_start(BaseUrl, Root)),
		  ?_test(test_create_bad_end(BaseUrl, Root)),
		  ?_test(test_create_unauthorized(BaseUrl, Ugly)),
		  
		  ?_test(test_get(BaseUrl, Root)),
		  ?_test(test_get_not_found_meeting(BaseUrl, Root)),
		  
		  ?_test(test_list_all(BaseUrl, Root)),
		  ?_test(test_list_upcoming(BaseUrl, Root)),
		  ?_test(test_list_closed(BaseUrl, Root)),
		  ?_test(test_list_open(BaseUrl, Root)),
		  ?_test(test_list_bad_parameters(BaseUrl, Root)),

		  ?_test(test_update(BaseUrl, Root)),
		  ?_test(test_update_not_found_meeting(BaseUrl, Root)),
		  ?_test(test_update_bad_start(BaseUrl, Root)),
		  ?_test(test_update_bad_end(BaseUrl, Root)),
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
               , {"start", integer_to_list(utils:now())}
               , {"metadata[description]", "Meeting"}],
    {struct, [{"result", "created"}]} = tests_utils:post(BaseUrl, "/meeting/all/", Params).

test_create_conflict(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
               , {"name", "newmeeting"}
	     , {"start", integer_to_list(utils:now())}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "conflict"}]} = tests_utils:post(BaseUrl, "/meeting/all/", Params).

test_create_bad_start(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
               , {"name", "newmeeting2"}
               , {"start", "i wish i was an integer"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "bad_parameters"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/", Params).

test_create_bad_end(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
               , {"name", "newmeeting2"}
               , {"end", "i wish i was an integer"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "bad_parameters"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/", Params).

test_create_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid}
             , {"sid", UglySid}
               , {"name", "newmeeting2"}
               , {"start", "0"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/", Params).


test_get(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}],
    {struct, [{"result",
	       {struct,
		[{"name", "newmeeting"},
         {"domain", _},
		 {"start_date",_},
		 {"end_date","never"},
		 {"metadata",{struct, [{"description", "Meeting"}]}}]}}]} =
	tests_utils:get(BaseUrl, "/meeting/all/newmeeting", Params).

test_get_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:get(BaseUrl, "/meeting/all/unexistentmeeting", Params).

test_list_all(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    JSON = tests_utils:get(BaseUrl, "/meeting/all", Params),
    test_meeting_in_list(["testmeeting"], JSON),
    test_meeting_in_list(["closedmeeting"], JSON),
    test_meeting_in_list(["upcomingmeeting"], JSON).

test_list_upcoming(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    JSON = tests_utils:get(BaseUrl, "/meeting/upcoming", Params),
    test_meeting_not_in_list(["testmeeting"], JSON),
    test_meeting_not_in_list(["closedmeeting"], JSON),
    test_meeting_in_list(["upcomingmeeting"], JSON).

test_list_closed(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
	       , {"sid", RootSid}],
    JSON = tests_utils:get(BaseUrl, "/meeting/closed", Params),
    test_meeting_not_in_list(["testmeeting"], JSON),
    test_meeting_in_list(["closedmeeting"], JSON),
    test_meeting_not_in_list(["upcomingmeeting"], JSON).

test_list_open(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    JSON = tests_utils:get(BaseUrl, "/meeting/opened", Params),
    test_meeting_in_list(["testmeeting"], JSON),
    test_meeting_not_in_list(["closedmeeting"], JSON),
    test_meeting_not_in_list(["upcomingmeeting"], JSON).

test_list_bad_parameters(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct, [{"error", "bad_parameters"}]} =
	tests_utils:get(BaseUrl, "/meeting/fishy_parameter", Params).

test_update(BaseUrl, {RootUid, RootSid}) ->
    Now = utils:now(),
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}
               , {"start", integer_to_list(Now)}
               , {"metadata[description]", "A new description"}],
    {struct, [{"result", "ok"}]} =
        tests_utils:put(BaseUrl, "/meeting/all/testmeeting", Params),
    {struct, [{"result",
	       {struct,
		[{"name", "testmeeting"},
         {"domain", _},
		 {"start_date", Now},
		 {"end_date","never"},
		 {"metadata",{struct, [{"description", "A new description"}]}}]}}]} =
	tests_utils:get(BaseUrl, "/meeting/all/testmeeting", Params).

test_update_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Now = integer_to_list(utils:now()),
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
               , {"start", Now}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:put(BaseUrl, "/meeting/all/unexistentmeeting", Params).

test_update_bad_start(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
	     , {"start", "i wish i was an integer"}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "bad_parameters"}]} =
	tests_utils:put(BaseUrl, "/meeting/all/testmeeting", Params).

test_update_bad_end(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
             , {"sid", RootSid}
	     , {"end", "i wish i was an integer"}
             , {"metadata[description]", "A new description"}],
    {struct, [{"error", "bad_parameters"}]} =
	tests_utils:put(BaseUrl, "/meeting/all/testmeeting", Params).

test_update_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid}
             , {"sid", UglySid}
               , {"start", "0"}
             , {"metadata[description]", "Meeting"}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:put(BaseUrl, "/meeting/all/testmeeting", Params).


test_join(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}],
    
    {struct, [{"result", "ok"}]} =
	tests_utils:post(BaseUrl, "/meeting/all/testmeeting/roster/", Params),

    {struct, [{"result", {array, Array}}]} = 
        tests_utils:get(BaseUrl, "/meeting/all/testmeeting/roster/", Params),
    [{struct,[{"uid",RootUid},
              {"domain",_},
              {"auth","password"},
              {"metadata",{struct,[]}}]}] = Array.

test_join_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
	       , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/unexistentmeeting/roster/", Params).

test_join_not_found_uid(BaseUrl) ->
    Params = [ {"uid", "unexistentuid"},
               {"sid", ""}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/testmeeting/roster/", Params).

test_join_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [{"uid", UglyUid},
              {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/meeting/all/testmeeting/roster/", Params).

test_leave(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
               , {"sid", RootSid}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/meeting/all/testmeeting/roster/" ++ RootUid, Params),
    {struct, [{"result", {array, Array}}]} =
        tests_utils:get(BaseUrl, "/meeting/all/testmeeting/roster", Params),
    [] = Array.

test_leave_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
	       , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/meeting/all/unexistentmeeting/roster/" ++ RootUid, Params).

test_leave_not_found_uid(BaseUrl, {RootUid, RootSid}) ->
    Params = [ {"uid", RootUid}
	       , {"sid", RootSid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/meeting/all/testmeeting/roster/unexistentuid", Params).

test_leave_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    Params = [ {"uid", UglyUid},
               {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/meeting/all/testmeeting/roster/test.user@af83.com", Params).

test_meeting_in_list(Id, {struct, [{"result", {array, List}}]}) ->
    test_meeting_in_list(Id, List);
test_meeting_in_list(Id, []) ->
    throw({not_found, Id});
test_meeting_in_list(Id, [Meeting|Tail]) ->
    {struct,
     [{"name", MeetingName},
      {"domain", _},
      {"start_date",_},
      {"end_date",_},
      {"metadata",_}]} = Meeting,
    case Id of
	[MeetingName] ->
	    true;
	_ ->
	    test_meeting_in_list(Id, Tail)
    end.

test_meeting_not_in_list(Id, JSON) ->
    case catch test_meeting_in_list(Id, JSON) of
	{not_found, _} ->
	    true;
	_ ->
	    throw({error, found})
    end.
