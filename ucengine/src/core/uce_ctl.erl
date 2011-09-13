%%
%%  U.C.Engine - Unified Collaboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(uce_ctl).

-export([start/0, stop/0, cmd/2]).

-export([infos/3, meeting/4, user/3, user/4, user/5, user/6, role/4, role/7, time/2]).

-export([parse_date/1, timestamp_to_iso/0, timestamp_to_iso/1]).

-compile({no_auto_import,[error/1]}).

-include("uce.hrl").

get_node() ->
    Command = init:get_arguments(),
    case utils:get(Command, ['-node']) of
        [none] ->
            NodeDomain =
                case re:run(atom_to_list(node()), "@(.*)", [{capture, all, list}]) of
                    {match, [_, Domain]} ->
                        Domain;
                    _ ->
                        "localhost"
                end,
            list_to_atom("ucengine@" ++ NodeDomain);
        [[Node]] ->
            list_to_atom(Node)
    end.

args_to_dictionary([]) ->
    [];
args_to_dictionary([{Key, Value}|Tail]) when is_atom(Key) ->
    args_to_dictionary([{atom_to_list(Key), Value}] ++ Tail);
args_to_dictionary([{"-" ++ Key, Value} | Tail]) ->
    [{Key, string:join(Value, " ")}] ++ args_to_dictionary(Tail);
args_to_dictionary([_|Tail]) ->
    [] ++ args_to_dictionary(Tail).

filter_node({"node", _Value}) ->
    false;
filter_node({_Name, _Value}) ->
    true.

%%
%% Parse date
%%
parse_date([Date, Time]) when is_list(Date), is_list(Time) ->
    parse_date(Date ++ " " ++ Time);

parse_date("0") ->
    0;
parse_date(Datetime) when is_list(Datetime) ->
    case string:tokens(Datetime, "- :") of
        [Year, Month, Day, Hours, Minutes, Seconds] ->
            DateTime = {{list_to_integer(Year),
                         list_to_integer(Month),
                         list_to_integer(Day)},
                        {list_to_integer(Hours),
                         list_to_integer(Minutes),
                         list_to_integer(Seconds)}},
            Epoch =
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            (calendar:datetime_to_gregorian_seconds(DateTime) - Epoch) * 1000;
        _ ->
            throw({error, bad_date})
    end;
parse_date(none) ->
    0;
parse_date(0) ->
    0.

timestamp_to_iso() ->
    timestamp_to_iso(utils:now()).
timestamp_to_iso(Militimestamp) when is_integer(Militimestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Timestamp = Epoch + (Militimestamp div 1000),
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(Timestamp),
    Date = io_lib:format("~p-~p-~p ~p:~p:~p"
                         , [Year, Month, Day, Hours, Minutes, Seconds]),
    lists:flatten(Date).

get_user_uid(Domain, Name) ->
    {ok, #uce_user{id=Uid}} = call(user, get_by_name, [Domain, Name]),
    {ok, Uid}.

success(Result) when is_list(Result) ->
    io:format("Success: ~s", [Result]),
    ok;
success(Result) ->
    io:format("Success: ~p", [Result]),
    ok.

error(Reason) ->
    io:format("Error: ~p~n", [Reason]),
    error.

start() ->
    Command = init:get_arguments(),
    Args = lists:filter(fun filter_node/1, args_to_dictionary(Command)),
    try cmd(proplists:lookup(dummy, Command), Args) of
        ok ->
            io:format("~n"),
            init:stop(0);
        error ->
            io:format("~n"),
            init:stop(2);
        {ok, nothing} ->
            init:stop(0);
        {ok, Result} ->
            io:format(Result, []),
            init:stop(0)
    catch
        error:_Reason ->
            usage();
        {error, nodedown} ->
            io:format("Fatal: U.C.Engine node is not running, call 'ucengine start' to start it.~n");
        Exception ->
            io:format("Fatal: ~p~n", [Exception]),
            init:stop(2)
    end,
    halt().

cmd({dummy, [Domain, Object, Action|Other]}, Args) ->
    Fun = list_to_atom(Object),
    apply(?MODULE, Fun, [Domain, Action]++Other ++ [Args]);
cmd({dummy, [Object, Action]}, Args) ->
    Fun = list_to_atom(Object),
    ?MODULE:Fun(Action, Args).

stop() ->
    ok.

usage() ->
    io:format("Usage:~n"),
    io:format("ucengine-admin <domain> <object> <action> [--<parameter> <value>]~n~n"),

    io:format("\tinfos get~n"),
    io:format("\tinfos update [--<parameter> <value>]~n~n"),

    io:format("Meetings:~n"),
    io:format("\tmeeting add <name> [--start <date>] [--end <date>] [--<metadata> <value>]~n"),
    io:format("\tmeeting update <name> [--start <date>] [--end <date>] [--<metadata> <value>]~n"),
    io:format("\tmeeting get <name>~n"),
    io:format("\tmeeting delete <name>~n"),
    io:format("\tmeeting list <status>~n~n"),

    io:format("Users:~n"),
    io:format("\tuser add <name> <auth> <credential> [--<metadata> <value>]~n"),
    io:format("\tuser update <name> <auth> <credential> [--<metadata> <value>]~n"),
    io:format("\tuser get <name>~n"),
    io:format("\tuser delete <name>~n"),
    io:format("\tuser list~n"),
    io:format("\tuser role add <name> <role> [--location <location>]~n"),
    io:format("\tuser role delete <name> <role> [--location <location>]~n~n"),

    io:format("Roles:~n"),
    io:format("\trole add <name>~n"),
    io:format("\trole delete <name>~n"),
    io:format("\trole access add <name> <action> <object> [--<condition> <value>]~n"),
    io:format("\trole access delete <name> <action> <object> [--<condition> <value>]~n"),
    io:format("\trole access check <name> <action> <object> [--<condition> <value>]~n~n"),

    io:format("ucengine-admin time get~n~n"),

    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-12-25 00:00:01')~n~n"),
    io:format("U.C.Engine (c) AF83 - http://ucengine.org~n"),
    {ok, nothing}.

call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    case catch rpc:call(get_node(), Module, Action, Args) of
        {badrpc, Reason} ->
            throw({error, Reason});
        {error, Reason} ->
            throw({error, Reason});
        Result ->
            Result
    end.

%%
%% Infos get
%%
infos(Domain, "get", _) ->
    {ok, Infos} = call(infos, get, [Domain]),
    {ok, pretty_print:print(Infos, flat)};

%%
%% Infos update
%%
infos(_Domain, "update", []) ->
    error(missing_parameter);
infos(Domain, "update", Metadata) ->
    {ok, updated} = call(infos, update, [Domain, #uce_infos{domain=Domain, metadata={struct, Metadata}}]),
    success(updated).

%%
%% Meeting add
%%
meeting(Domain, "add", Name, Args) ->
    Start = proplists:get_value("start", Args, 0),
    End = proplists:get_value("end", Args, 0),

    {_, Metadata} =  proplists:split(Args, ["start", "end"]),
    {ok, created} = call(meeting, add, [Domain,
                                        #uce_meeting{id=Name,
                                                     start_date=parse_date(Start),
                                                     end_date=parse_date(End),
                                                     metadata={struct, Metadata}}]),
    success(created);

%%
%% Meeting update
%%
meeting(Domain, "update", Name, Args) ->
    Start = proplists:get_value("start", Args, 0),
    End = proplists:get_value("end", Args, 0),

    {_, Metadata} =  proplists:split(Args, ["start", "end"]),
    {ok, updated} = call(meeting, update, [Domain,
                                           #uce_meeting{id=Name,
                                                        start_date=parse_date(Start),
                                                        end_date=parse_date(End),
                                                        metadata={struct, Metadata}}]),
    success(updated);

%%
%% Meeting delete
%%
meeting(Domain, "delete", Name, []) ->
    {ok, deleted} = call(meeting, delete, [Domain, Name]),
    success(deleted);

%%
%% Meeting get
%%
meeting(Domain, "get", Name, []) ->
    {ok, Record} = call(meeting, get, [Domain, Name]),
    {ok, pretty_print:print(Record, flat)};

%%
%% Meeting list
%%
meeting(Domain, "list", Status, []) ->
    {ok, Records} = call(meeting, list, [Domain, Status]),
    {ok, pretty_print:print(Records, flat)}.

%%
%% Users
%%
user(Domain, "add", Name, Auth, Credential, Metadata) ->
    {ok, Uid} = call(user, add, [Domain,
                                 #uce_user{id=none,
                                           name=Name,
                                           auth=Auth,
                                           credential=Credential,
                                           metadata={struct, Metadata}}]),
    success(Uid);

%%
%% User update
%%
user(Domain, "update", Name, Auth, Credential, Metadata) ->
    {ok, Uid} = get_user_uid(Domain, Name),
    {ok, updated} = call(user, update, [Domain,
                                        #uce_user{id=Uid,
                                                  name=Name,
                                                  auth=Auth,
                                                  credential=Credential,
                                                  metadata={struct, Metadata}}]),
    success(updated);

%%
%% User add role
%%
user(Domain, "role", "add", Name, Role, Args) ->
    {ok, Uid} = get_user_uid(Domain, Name),
    Location = proplists:get_value("location", Args, ""),
    {ok, updated} = call(user, add_role, [Domain,
                                          Uid,
                                          {Role, Location}]),
    success(updated);

%%
%% User delete role
%%
user(Domain, "role", "delete", Name, Role, Args) ->
    {ok, Uid} = get_user_uid(Domain, Name),
    Location = proplists:get_value("location", Args, ""),
    {ok, updated} = call(user, delete_role, [Domain,
                                             Uid,
                                             {Role, Location}]),
    success(Uid).

%%
%% Anonymous user add
%%
user(Domain, "add", Name, Auth, Metadata) ->
    {ok, Uid} = call(user, add, [Domain,
                                 #uce_user{id=none,
                                           name=Name,
                                           auth=Auth,
                                           metadata=json_helpers:to_struct(Metadata)}]),
    success(Uid).

%%
%% User delete
%%
user(Domain, "delete", Name, []) ->
    {ok, #uce_user{id=Uid}} = call(user, get, [Domain, Name]),
    {ok, deleted} = call(user, delete, [Domain, Uid]),
    success(deleted);

%%
%% User get
%%
user(Domain, "get", Name, []) ->
    {ok, Record} = call(user, get, [Domain, Name]),
    {ok, pretty_print:print(Record, flat)}.

%%
%% User list
%%
user(Domain, "list", []) ->
    {ok, Records} = call(user, list, [Domain]),
    {ok, pretty_print:print(Records, flat)}.

%%
%% Role add
%%
role(Domain, "add", Name, []) ->
    {ok, created} = call(role, add, [Domain, #uce_role{id=Name}]),
    success(created);

%%
%% Role delete
%%
role(Domain, "delete", Name, []) ->
    {ok, deleted} = call(role, delete, [Domain, Name]),
    success(deleted).

%%
%% Role access add
%%
role(Domain, "access", "add", Name, Action, Object, Conditions) ->
    {ok, updated} = call(role, add_access, [Domain, Name,
                                            #uce_access{action=Action,
                                                        object=Object,
                                                        conditions=Conditions}]),
    success(updated);

%%
%% Role access delete
%%
role(Domain, "access", "delete", Name, Action, Object, Conditions) ->
    {ok, updated} = call(role, delete_access, [Domain, Name,
                                               #uce_access{action=Action,
                                                           object=Object,
                                                           conditions=Conditions}]),
    success(updated);

%%
%% Role access check
%%
role(Domain, "access", "check", Name, Action, Object, Args) ->
    {ok, Uid} = get_user_uid(Domain, Name),
    Location = proplists:get_value("location", Args, ""),

    {_, Conditions} = proplists:split(Args, ["location"]),
    {ok, Result} = call(access, check, [Domain,
                                        Uid,
                                        Location,
                                        Object,
                                        Action,
                                        Conditions]),
    success(Result).

%%
%% Time
%%
time("get", []) ->
    io:format("Server time: ~p", [utils:now()]),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_arguments_test() ->
    ?assertEqual([], args_to_dictionary([])),
    ?assertEqual([{"node", "plop"}], args_to_dictionary([{'-node', ["plop"]}])),
    ?assertEqual([{"node", "plop plip"}], args_to_dictionary([{'-node', ["plop", "plip"]}])),
    ?assertEqual([{"node", "plop plip"}, {"name", "chuck"}], args_to_dictionary([{'-node', ["plop", "plip"]}, {'-name', ["chuck"]}])),
    ?assertEqual([{"node", "plop"}], args_to_dictionary([{'-node', ["plop"]}, {hello, "plop"}])).

parse_date_test() ->
    ?assertEqual(0, parse_date(0)),
    ?assertEqual(0, parse_date("0")),
    ?assertEqual(0, parse_date(none)),
    ?assertEqual(1306139400000, parse_date("2011-05-23 08:30:00")),
    ?assertEqual(1306139400000, parse_date(["2011-05-23", "08:30:00"])).

timestamp_to_iso_test() ->
    ?assertEqual("2011-5-23 8:30:0", timestamp_to_iso(1306139400000)).

-endif.
