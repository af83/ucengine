%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2010 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2010 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2010 Rabbit Technologies Ltd.
%%
%%   Portions created by AF83 are Copyright (C) 2010 AF83.
%%
%%   All Rights Reserved.

-module(uce_ctl).

-author('victor.goya@af83.com').

-export([start/0, stop/0, getopt/2]).

-include("uce.hrl").

args_to_dictionary([]) ->
    [];
args_to_dictionary([[_, _ | Key], Value | Tail]) ->
    [{Key, Value}] ++ args_to_dictionary(Tail).

start() ->
    FullCommand = init:get_plain_arguments(),
    case FullCommand of
        [] ->
	    usage();
        _ ->
	    ok
    end,
    [Object, Action | Args] = FullCommand,
    case catch action(list_to_atom(Object), list_to_atom(Action), args_to_dictionary(Args)) of
	{ok, Result} ->
	    io:format("Success: ~p~n", [Result]);
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason]);
	Exception ->
	    io:format("Fatal: " ++ Exception ++ "~n")
    end,
    halt().

stop() ->
    ok.

usage() ->
    nothing.

getopt(Keys, []) ->
    {[], []};
getopt(Keys, [{Key, Value} | Tail]) ->
    {Wanted, Remaining} = getopt(Keys, Tail),
    case lists:member(Key, Keys) of
	true ->
	    {[Value] ++ Wanted, Remaining};
	false ->
	    {Wanted, [{Key, Value}] ++ Remaining}
    end.
	    
call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), Module, Action, [Args]).

format_field([], []) ->
    [];
format_field([Value|Values], [Field|Fields]) ->
    io_lib:format("~p: ~p~n", [Field, Value]) ++ format_field(Values, Fields).

format(Record, Fields) ->
    [_|Values] = tuple_to_list(Record),
    format_field(Values, Fields).

action(org, add, Args) ->
    {[Name], Metadata} = getopt(["name"], Args),
    call(org, add, #uce_org{name=Name, metadata=Metadata});

action(org, get, Args) ->
    NodeStr = "ucengine@localhost",
    {[Name], Metadata} = getopt(["name"], Args),
    Response = call(org, get, Name),
    format(Response, record_info(fields, uce_org)).
