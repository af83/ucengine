-module(uce_event_test_search).

-export([add/2, setup_full_text/1]).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").


add(_Domain, Event) ->
    ets:insert(uce_event_test_search, {Event#uce_event.id, Event}),
    {ok, created}.

setup_full_text(Domain) ->
    EventManager = uce_meeting:get_event_manager(Domain, ""),
    gen_event:add_handler(EventManager, uce_search_handler, [Domain]),
    ok.

