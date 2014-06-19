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
-module(mongodb_helpers).

-include("uce.hrl").

-export([to_bson/1, ok/1, updated/1, collection_to_list/1]).

-include_lib("eunit/include/eunit.hrl").

to_bson([]) ->
    [];
to_bson({struct, Proplist}) ->
    to_bson(Proplist, []);
to_bson({array, Values}) ->
    {array, to_bson(Values, [])};
to_bson({Name, {struct, Values}}) ->
    {Name, to_bson(Values, [])};
to_bson({Name, {array, Values}}) ->
    {Name, {array, to_bson(Values, [])}};
to_bson({Name, Value}) ->
    {Name, Value};
to_bson(Value) ->
    Value.

to_bson([Value|Values], Acc) ->
    to_bson(Values, [to_bson(Value)|Acc]);
to_bson([], Acc) ->
    lists:reverse(Acc).

ok([Result]) ->
    case proplists:lookup(<<"err">>, Result) of
        {<<"err">>, null} ->
            ok;
        {<<"err">>, Err} ->
            ?ERROR_MSG("mongodb error ~p", [Err]),
            error
    end.

updated(Result) ->
    ok(Result).

collection_member_to_list({array, Value}) when is_list(Value) ->
    {array, [collection_member_to_list(Elem) || Elem <- Value]};

collection_member_to_list(Value) when is_tuple(Value) ->
    tuple_to_list(Value);

collection_member_to_list(Value) when is_integer(Value) ->
    Value;

collection_member_to_list(Value) when is_list(Value) ->
    {struct, collection_to_list(Value)};

collection_member_to_list(Value) when is_binary(Value) ->
    unicode:characters_to_list(Value);

collection_member_to_list(Value) when is_float(Value) ->
    Value;

collection_member_to_list(Value) when is_atom(Value) ->
    Value.

%%--------------------------------------------------------------------
%% @spec ([{Key::binary, Value::Binary}, {Key::binary, Value::Binary}, ...] = Collection::list) -> [{Key::list, Value::list}, {Key::binary, Value:Binary}, ...] = NewCollection::list
%% @doc Convert list of tuple of two binaries returned by mongodb to list of tuple of 2 list
%% @end
%%--------------------------------------------------------------------
collection_to_list(Collection) ->
    lists:map(fun({Key, Value}) ->
                      {collection_member_to_list(Key), collection_member_to_list(Value)}
              end,
              Collection).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

collection_to_list_test() ->
    ?assertEqual([{"id", "42"}], collection_to_list([{<<"id">>,<<"42">>}])),
    ?assertEqual([{"nickname", ""}], collection_to_list([{<<"nickname">>,<<>>}])),
    ?assertEqual([{"_id", [oid, <<"root">>]}], collection_to_list([{<<"_id">>,{oid,<<"root">>}}])).

to_bson_test() ->
    ?assertEqual([],
                 to_bson([])),

    ?assertEqual([{"name", "plop"}],
                 to_bson({struct, [{"name", "plop"}]})),

    ?assertEqual({array, [{"name", "plop"}]},
                 to_bson({array, [{"name", "plop"}]})),

    ?assertEqual({array, [10, [{"name", "plop"}]]},
                 to_bson({array, [10, {struct, [{"name", "plop"}]}]})),

    ?assertEqual([{"name", {array, ["plop", "plip"]}}],
                 to_bson({struct, [{"name", {array, ["plop", "plip"]}}]})),

    ?assertEqual([{"name", [{"plop", "plip"}]}],
                 to_bson({struct, [{"name", {struct, [{"plop", "plip"}]}}]})),

    ?assertEqual([{"name", [{"plop", {array, ["plip", "plaf"]}}]}],
                 to_bson({struct, [{"name", {struct, [{"plop", {array, ["plip", "plaf"]}}]}}]})),

    ?assertEqual({array, [10, [{"name", "plip"}]]},
                 to_bson({array, [10, {struct, [{"name", "plip"}]}]})),

    ?assertEqual([{"complex", {array, [10, [{"name", "plip"}]]}}],
                 to_bson({struct, [{"complex", {array, [10, {struct, [{"name", "plip"}]}]}}]})),

    ?assertEqual([{"score", null}],
                 to_bson({struct, [{"score", null}]})),
    ?assertEqual([{"score", true}],
                 to_bson({struct, [{"score", true}]})),
    ?assertEqual([{"score", false}],
                 to_bson({struct, [{"score", false}]})).

-endif.
