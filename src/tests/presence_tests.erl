-module(presence_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

presence_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(_) ->
        [ ?_test(test_presence_create_password())
        , ?_test(test_presence_create_bad_password())
	  , ?_test(test_presence_create_not_found_org())
	  
	  , ?_test(test_presence_create_anonymous())
	  
	  , ?_test(test_presence_close())
	  , ?_test(test_presence_close_not_founduid())
	  , ?_test(test_presence_close_not_foundsid())
	  , ?_test(test_presence_close_not_found_org())
        ]
      end
    }.

test_presence_create_password() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    {struct,[{"result", _}]} = tests_utils:put("/presence/testorg/participant.user@af83.com", Params).

test_presence_create_bad_password() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	     {"credential", "badpwd"}],
    {struct,[{"error", "bad_credentials"}]} = tests_utils:put("/presence/testorg/participant.user@af83.com", Params).

test_presence_create_not_found_org() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    {struct,[{"error", "not_found"}]} = tests_utils:put("/presence/unexistentorg/participant.user@af83.com", Params).

test_presence_create_anonymous() ->
    Params = [{"metadata[nickname]", "AnonymousParticipant"},
	      {"auth", "anonymous"}],
    {struct,[{"result", _}]} = tests_utils:put("/presence/testorg/anonymous.user@af83.com", Params).

test_presence_close() ->
    Params = [ {"metadata[nickname]", "PasswordParticipant"}
             , {"auth", "password"}
	     , {"credential", "pwd"}
             ],
    EUid = "participant.user@af83.com",
    {struct,[{"result", ESid}]} = tests_utils:put("/presence/testorg/" ++ EUid, Params),

    ParamsDelete = [{"uid", EUid},
		    {"sid", ESid}],
    {struct, [{"result", "ok"}]} = tests_utils:delete("/presence/testorg/" ++ EUid ++ "/" ++ ESid, ParamsDelete).

test_presence_close_not_founduid() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    EUid = "participant.user@af83.com",
    {struct,[{"result", ESid}]} = tests_utils:put("/presence/testorg/" ++ EUid, Params),

    ParamsDelete = [{"uid", EUid},
		    {"sid", ESid}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/presence/testorg/unexistentuid/" ++ ESid, ParamsDelete).

test_presence_close_not_foundsid() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],
    EUid = "participant.user@af83.com",
    {struct,[{"result", ESid}]} = tests_utils:put("/presence/testorg/" ++ EUid, Params),

    ParamsDelete = [{"uid", EUid},
		    {"sid", ESid}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/presence/testorg/" ++ EUid ++ "/unexistentsid", ParamsDelete).

test_presence_close_not_found_org() ->
    Params = [{"metadata[nickname]", "PasswordParticipant"},
	      {"auth", "password"},
	      {"credential", "pwd"}],

    EUid = "participant.user@af83.com",
    {struct,[{"result", ESid}]} = tests_utils:put("/presence/testorg/" ++ EUid, Params),

    ParamsDelete = [{"uid", EUid},
		    {"sid", ESid}],
    {struct, [{"error", "not_found"}]} = tests_utils:delete("/presence/unexistentorg/" ++ EUid ++ "/" ++ ESid, ParamsDelete).
