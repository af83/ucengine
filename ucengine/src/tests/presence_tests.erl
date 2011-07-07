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
-module(presence_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

presence_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([_, BaseUrl, [_Root, _Participant, Ugly|_]]) ->
              [ ?_test(test_presence_create_password(BaseUrl)),
                ?_test(test_presence_create_with_no_password(BaseUrl)),
                ?_test(test_presence_create_missing_credential(BaseUrl)),
                ?_test(test_presence_create_bad_password(BaseUrl)),
                ?_test(test_presence_create_not_found_user(BaseUrl)),
                ?_test(test_presence_create_unauthorized(BaseUrl)),

                ?_test(test_presence_get(BaseUrl)),
                ?_test(test_presence_get_not_found(BaseUrl)),

                ?_test(test_presence_close(BaseUrl)),
                ?_test(test_presence_close_unauthorized(BaseUrl, Ugly)),
                ?_test(test_presence_close_not_foundsid(BaseUrl)),
                ?_test(test_presence_timeout(BaseUrl)),
                ?_test(test_multiple_presence_timeout(BaseUrl))]
      end
    }.

create_presence(BaseUrl, Name) ->
    Params = [{"name", Name}],
    tests_utils:post(BaseUrl, "/presence/", Params).
create_presence(BaseUrl, Name, Credential) ->
    create_presence(BaseUrl, Name, Credential, []).
create_presence(BaseUrl, Name, Credential, Metadata) ->
    Params = [{"name", Name},
              {"credential", Credential}] ++ Metadata,
    tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_password(BaseUrl) ->
    {struct,[{"result", _}]} = create_presence(BaseUrl, "participant.user@af83.com", "pwd").

test_presence_create_with_no_password(BaseUrl) ->
    {struct,[{"result", _}]} = create_presence(BaseUrl, "anonymous.user@af83.com").

test_presence_create_missing_credential(BaseUrl) ->
    {struct,[{"error", "bad_credentials"}]} = create_presence(BaseUrl, "participant.user@af83.com").

test_presence_create_bad_password(BaseUrl) ->
    {struct,[{"error", "bad_credentials"}]} = create_presence(BaseUrl, "participant.user@af83.com", "badpwd").

test_presence_create_not_found_user(BaseUrl) ->
    {struct,[{"error", "not_found"}]} = create_presence(BaseUrl, "unexistent.user@af83.com", "pwd").

test_presence_create_unauthorized(BaseUrl) ->
    {struct,[{"error", "unauthorized"}]} = create_presence(BaseUrl, "ugly.user@af83.com", "pwd").

test_presence_get(BaseUrl) ->
    Uid = "participant.user@af83.com",
    {struct,[{"result", {struct, [{"uid", _}, {"sid", Sid}]}}]} =
        create_presence(BaseUrl, Uid, "pwd", [{"metadata[nickname]", "PasswordParticipantGet"}]),

    {struct,[{"result",
              {struct,[{"id",Sid},
                       {"domain",_},
                       {"user",_}, %%"participant.user@af83.com"},
                       {"auth","password"},
                       {"metadata", {struct, [{"nickname", "PasswordParticipantGet"}]}}]}}]} =
        tests_utils:get(BaseUrl, "/presence/" ++ Sid).

test_presence_get_not_found(BaseUrl) ->
    {struct,[{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/presence/unexistent_sid").

test_presence_close(BaseUrl) ->
    Uid = "participant.user@af83.com",
    {struct,[{"result", {struct, [{"uid", Id}, {"sid", Sid}]}}]} = create_presence(BaseUrl, Uid, "pwd"),

    ParamsDelete = [{"uid", Id},
                    {"sid", Sid}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/presence/" ++ Sid, ParamsDelete).

test_presence_close_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    ParamsDelete = [{"uid", UglyUid},
                    {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/presence/" ++ UglySid, ParamsDelete).

test_presence_close_not_foundsid(BaseUrl) ->
    {struct,[{"result", {struct, [{"uid", Id},
                                  {"sid", Sid}]}}]} = create_presence(BaseUrl, "participant.user@af83.com", "pwd"),

    ParamsDelete = [{"uid", Id},
                    {"sid", Sid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/presence/unexistentsid", ParamsDelete).

test_presence_timeout(BaseUrl) ->
    DefaultTimeout = config:get(timeout_refresh),
    {struct,[{"result", {struct, [{"uid", Uid}, {"sid", Sid}]}}]} =
        create_presence(BaseUrl, "participant.user@af83.com", "pwd", [{"timeout", integer_to_list(DefaultTimeout)}]),
    timer:sleep(DefaultTimeout * 2000),
    ParamsGet = [{"uid", Uid},
                 {"sid", Sid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/presence/" ++ Sid, ParamsGet).

%%
%% Test the case when a user have multiple session on the same meeting
%% We should not cleanup meeting in this case
%%
test_multiple_presence_timeout(BaseUrl) ->
    {timeout, 120,
          fun() ->
                  DefaultTimeout = config:get(timeout_refresh),
                                                % Create first presence and join a meeting
                  {struct,[{"result", {struct, [{"uid", Uid}, {"sid", Sid}]}}]} =
                      create_presence(BaseUrl, "participant.user@af83.com", "pwd", [{"timeout", integer_to_list(DefaultTimeout)}]),
                  Params = [ {"uid", Uid}
                             , {"sid", Sid}],
                  ?assertMatch({struct, [{"result", "ok"}]}, tests_utils:post(BaseUrl, "/meeting/all/testmeeting/roster/", Params)),
                  ?assertMatch({struct, [{"result", "ok"}]}, tests_utils:post(BaseUrl, "/meeting/all/closedmeeting/roster/", Params)),
                                                % Create second presence and join the same meeting
                  {struct,[{"result", {struct, [{"uid", Uid}, {"sid", Sid2}]}}]} =
                      create_presence(BaseUrl, "participant.user@af83.com", "pwd", [{"timeout", integer_to_list(DefaultTimeout * 15000)}]),
                  Params2 = [ {"uid", Uid}
                              , {"sid", Sid2}],
                  ?assertMatch({struct, [{"result", "ok"}]}, tests_utils:post(BaseUrl, "/meeting/all/testmeeting/roster/", Params2)),
                  %% Manual cleanup
                  timer:sleep(DefaultTimeout * 1000),
                  uce_timeout:force("localhost"),
                  ?assertMatch({struct, [{"error", "not_found"}]}, tests_utils:get(BaseUrl, "/presence/" ++ Sid, Params)),
                  ?assertMatch({struct, [{"result", _}]}, tests_utils:get(BaseUrl, "/presence/" ++ Sid2, Params2)),
                  {struct, [{"result", {array, Array}}]} =
                      tests_utils:get(BaseUrl, "/meeting/all/testmeeting/roster/", Params2),
                  %% We should be here
                  [{struct,[{"uid", Uid},
                            {"name",_},
                            {"domain",_},
                            {"auth","password"},
                            {"metadata",{struct,[]}}]}] = Array,
                  ?assertMatch({struct, [{"result", {array, []}}]},
                               tests_utils:get(BaseUrl, "/meeting/all/closedmeeting/roster/", Params2))
          end}.
