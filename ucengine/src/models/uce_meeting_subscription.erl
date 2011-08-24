-module(uce_meeting_subscription).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

init([Subscriber]) ->
    % The Subscriber is assumed to be a gen_event process.
    {ok, Subscriber}.

handle_event(Event, Subscriber) ->
    gen_event:notify(Subscriber, Event),
    {ok, Subscriber}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

