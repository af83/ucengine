-module(uce_meeting_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

init([Pid]) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    % XXX: should we use the gen_server API instead?
    Pid ! Event,
    {ok, Pid}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

