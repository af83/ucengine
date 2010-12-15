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
	{ok, nothing} ->
	    nothing;
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
    io:format("Usage:~n"),
    {ok, nothing}.

getopt(_Keys, []) ->
    {[], []};
getopt(Keys, [{Key, Value} | Tail]) ->
    {Wanted, Remaining} = getopt(Keys, Tail),
    case lists:member(Key, Keys) of
	true ->
	    {[Value] ++ Wanted, Remaining};
	false ->
	    {Wanted, [{Key, Value}] ++ Remaining}
    end.

format_field([], []) ->
    [];
format_field([Metadata|Values], [metadata|Fields]) ->
    FormattedMetadata =
	[io_lib:format("metadata[~p]: ~ts", [Key, Value]) || {Key, Value} <- Metadata],
    string:join(FormattedMetadata, "~n") ++ format_field(Values, Fields);
format_field([Value|Values], [Field|Fields]) when is_list(Value) ->
    io_lib:format("~p: ~ts~n", [Field, unicode:characters_to_binary(Value)]) ++ format_field(Values, Fields);
format_field([Value|Values], [Field|Fields]) ->
    io_lib:format("~p: ~p~n", [Field, Value]) ++ format_field(Values, Fields).

format(Record, Fields) ->
    [_|Values] = tuple_to_list(Record),
    format_field(Values, Fields).
	    
call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), Module, Action, Args).

action(org, add, Args) ->
    {[Name], Metadata} = getopt(["name"], Args),
    call(org, add, [#uce_org{name=Name, metadata=Metadata}]);

action(org, get, Args) ->
    {[Name], _} = getopt(["name"], Args),
    Response = call(org, get, [Name]),
    format(Response, record_info(fields, uce_org));

action(org, update, Args) ->
    {[Name], Metadata} = getopt(["name"], Args),
    call(org, update, [#uce_org{name=Name, metadata=Metadata}]);

action(org, delete, Args) ->
    {[Name], _} = getopt(["name"], Args),
    call(org, update, [Name]);

action(demo, start, Args) ->
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), demo, start, Args);

action(_, _, _) ->
    usage().
