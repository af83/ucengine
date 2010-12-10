-module(password_auth).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([check/2]).

check(User, Credential) ->
    case User#uce_user.credential of
	Credential ->
	    true;
	_ ->
	    false
    end.
