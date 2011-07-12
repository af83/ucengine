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

-author('victor.goya@af83.com').

-export([ok/1, updated/1, collection_to_list/1]).

ok([Result]) ->
    case proplists:lookup(<<"err">>, Result) of
        {<<"err">>, undefined} ->
            ok;
        {<<"err">>, Err} ->
            ?ERROR_MSG("mongodb error ~p", [Err]),
            error
    end.

updated(Result) ->
    ok(Result).

collection_member_to_list({array, Value}) when is_list(Value) ->
    lists:map(fun(Elem) ->
                      collection_member_to_list(Elem)
              end,
              Value);

collection_member_to_list(Value) when is_tuple(Value) ->
    tuple_to_list(Value);

collection_member_to_list(Value) when is_integer(Value) ->
    Value;

collection_member_to_list(Value) when is_list(Value) ->
    collection_to_list(Value);

collection_member_to_list(Value) when is_binary(Value) ->
    unicode:characters_to_list(Value).

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

-endif.
