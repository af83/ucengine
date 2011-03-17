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
                ?_test(test_presence_create_unauthorized(BaseUrl, Ugly)),

                ?_test(test_presence_get(BaseUrl)),
                ?_test(test_presence_get_not_found(BaseUrl)),
                
                ?_test(test_presence_close(BaseUrl)),
                ?_test(test_presence_close_unauthorized(BaseUrl, Ugly)),
                ?_test(test_presence_close_not_foundsid(BaseUrl)),
                ?_test(test_presence_timeout(BaseUrl))]
      end
    }.

test_presence_create_password(BaseUrl) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", "participant.user@af83.com"},
              {"credential", "pwd"}],
    {struct,[{"result", _}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_with_no_password(BaseUrl) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", "anonymous.user@af83.com"}],
    {struct,[{"result", _}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_missing_credential(BaseUrl) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", "participant.user@af83.com"}],
    {struct,[{"error", "bad_credentials"}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_bad_password(BaseUrl) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", "participant.user@af83.com"},
              {"credential", "badpwd"}],
    {struct,[{"error", "bad_credentials"}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_not_found_user(BaseUrl) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", "unexistent.user@af83.com"},
              {"credential", "pwd"}],
    {struct,[{"error", "not_found"}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_create_unauthorized(BaseUrl, {UglyUid, _}) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", UglyUid},
              {"credential", "pwd"}],
    {struct,[{"error", "unauthorized"}]} =
        tests_utils:post(BaseUrl, "/presence/", Params).

test_presence_get(BaseUrl) ->
    Uid = "participant.user@af83.com",
    Params = [{"metadata[nickname]", "PasswordParticipantGet"},
              {"uid", Uid},
              {"credential", "pwd"}],
    {struct,[{"result", Sid}]} = tests_utils:post(BaseUrl, "/presence/", Params),

    {struct,[{"result",
              {struct,[{"id",Sid},
                       {"domain",_},
                       {"user","participant.user@af83.com"},
                       {"auth","password"},
                       {"metadata", {struct, [{"nickname", "PasswordParticipantGet"}]}}]}}]} =
        tests_utils:get(BaseUrl, "/presence/" ++ Sid, []).

test_presence_get_not_found(BaseUrl) ->
    {struct,[{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/presence/unexistent_sid", []).

test_presence_close(BaseUrl) ->
    Uid = "participant.user@af83.com",
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", Uid},
              {"credential", "pwd"}],
    {struct,[{"result", Sid}]} = tests_utils:post(BaseUrl, "/presence/", Params),

    ParamsDelete = [{"uid", Uid},
                    {"sid", Sid}],
    {struct, [{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/presence/" ++ Sid, ParamsDelete).

test_presence_close_unauthorized(BaseUrl, {UglyUid, UglySid}) ->
    ParamsDelete = [{"uid", UglyUid},
                    {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
        tests_utils:delete(BaseUrl, "/presence/" ++ UglySid, ParamsDelete).

test_presence_close_not_foundsid(BaseUrl) ->
    Uid = "participant.user@af83.com",
    Params = [{"metadata[nickname]", "PasswordParticipant"},
              {"uid", Uid},
              {"credential", "pwd"}],
    {struct,[{"result", Sid}]} = tests_utils:post(BaseUrl, "/presence/", Params),

    ParamsDelete = [{"uid", Uid},
                    {"sid", Sid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/presence/unexistentsid", ParamsDelete).

%% XXX: Timeout related tests can fail due to the config file.
%% Should we have a dedicated config file for the tests?
test_presence_timeout(BaseUrl) ->
    DefaultTimeout = 1,
    Params = [{"uid", "participant.user@af83.com"},
              {"timeout", integer_to_list(DefaultTimeout)},
              {"credential", "pwd"}],
    {struct,[{"result", Sid}]} =
        tests_utils:post(BaseUrl, "/presence/", Params),
    timer:sleep(DefaultTimeout * 2000),
    ParamsDelete = [{"uid", "participant.user@af83.com"},
                    {"sid", Sid}],
    {struct, [{"error", "not_found"}]} =
        tests_utils:delete(BaseUrl, "/presence/" ++ Sid, ParamsDelete).
