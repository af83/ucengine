-module(mongodb_helpers).

-author('victor.goya@af83.com').

-export([collection_to_list/1, get_item_from_collection/2]).

collection_member_to_list({array, Value}) when is_list(Value) ->
    lists:map(fun(Elem) ->
                      binary_to_list(Elem)
              end,
              Value);

collection_member_to_list(Value) when is_tuple(Value) ->
    tuple_to_list(Value);

collection_member_to_list(Value) when is_integer(Value) ->
    Value;

collection_member_to_list(Value) when is_list(Value) ->
    collection_to_list(Value);

collection_member_to_list(<<>>) ->
    [];

collection_member_to_list(Value) when is_binary(Value) ->
    binary_to_list(Value).

collection_to_list(Collection) ->
    lists:map(fun({Key, Value}) ->
		      {binary_to_list(Key), collection_member_to_list(Value)}
	      end,
	      Collection).

get_item_from_collection(Key, Collection) ->
    {_Key, Item} = lists:keyfind(list_to_binary(Key), 1, Collection),
    {Key, collection_member_to_list(Item)}.
