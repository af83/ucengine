-module(mnesia_db).

-author('victor.goya@af83.com').

-export([init/1,
         drop/0,
         terminate/0]).

-include("uce.hrl").

call_mnesia_modules(Fun) ->
    lists:foreach(fun(Module) ->
                          apply(list_to_atom(atom_to_list(Module) ++ "_mnesia"), Fun, [])
                  end,
                  [uce_acl, uce_user, uce_meeting, uce_file, uce_event, uce_presence, uce_infos]).

init(_) ->
    call_mnesia_modules(init),
    ok.

drop() ->
    call_mnesia_modules(drop),
    ok.

terminate() ->
    ok.
