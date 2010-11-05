-module(user_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([to_json/1]).

to_json(#uce_user{} = User) ->
    {struct, [{uid, User#uce_user.uid},
	      {auth, User#uce_user.auth},
	      {metadata, {struct, User#uce_user.metadata}}]}.

