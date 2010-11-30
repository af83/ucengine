-module(presence_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

presence_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(Testers) ->
        [ ?_test(test_presence_create_password()),
	  ?_test(test_presence_create_missing_auth()),
	  ?_test(test_presence_create_missing_credential()),
	  ?_test(test_presence_create_bad_password()),
	  ?_test(test_presence_create_not_found_user()),
	  ?_test(test_presence_create_unauthorized(Testers)),
	  
	  ?_test(test_presence_close()),
	  ?_test(test_presence_close_unauthorized(Testers)),
	  ?_test(test_presence_close_not_foundsid())]
      end
    }.

test_presence_create_password() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    {struct,[{"result", _}]} = tests_utils:put("/presence/participant.user@af83.com", Params).

test_presence_create_missing_auth() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"credential", "pwd"}],
    {struct,[{"error", "missing_parameters"}]} =
	tests_utils:put("/presence/participant.user@af83.com", Params).

test_presence_create_missing_credential() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"}],
    {struct,[{"error", "missing_parameters"}]} =
	tests_utils:put("/presence/participant.user@af83.com", Params).

test_presence_create_bad_password() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "badpwd"}],
    {struct,[{"error", "bad_credentials"}]} =
	tests_utils:put("/presence/participant.user@af83.com", Params).

test_presence_create_not_found_user() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    {struct,[{"error", "unauthorized"}]} =
	tests_utils:put("/presence/unexistent.user@af83.com", Params).

test_presence_create_unauthorized([_, {UglyUid, _}]) ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    {struct,[{"error", "unauthorized"}]} =
	tests_utils:put("/presence/" ++ UglyUid, Params).

test_presence_close() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    Uid = "participant.user@af83.com",
    {struct,[{"result", Sid}]} = tests_utils:put("/presence/" ++ Uid, Params),

    ParamsDelete = [{"uid", Uid},
		    {"sid", Sid}],
    {struct, [{"result", "ok"}]} =
	tests_utils:delete("/presence/" ++ Uid ++ "/" ++ Sid, ParamsDelete).

test_presence_close_unauthorized([_, {UglyUid, UglySid}]) ->
    ParamsDelete = [{"uid", UglyUid},
		    {"sid", UglySid}],
    {struct, [{"error", "unauthorized"}]} =
	tests_utils:delete("/presence/" ++ UglyUid ++ "/" ++ UglySid, ParamsDelete).

test_presence_close_not_foundsid() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    Uid = "participant.user@af83.com",
    {struct,[{"result", Sid}]} = tests_utils:put("/presence/" ++ Uid, Params),

    ParamsDelete = [{"uid", Uid},
		    {"sid", Sid}],
    {struct, [{"error", "not_found"}]} =
	tests_utils:delete("/presence/" ++ Uid ++ "/unexistentsid", ParamsDelete).
