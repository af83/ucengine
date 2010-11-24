-module(mongodb_helpers).

-author('victor.goya@af83.com').

-export([collection_to_list/1]).

collection_member_to_list({Key, {array, Value}})
  when is_list(Value) ->
    {binary_to_list(Key), lists:map(fun(Elem) ->
					    binary_to_list(Elem)
				    end,
				    Value)};

collection_member_to_list({Key, Value})
  when is_tuple(Value) ->
    {binary_to_list(Key), tuple_to_list(Value)};

collection_member_to_list({Key, Value})
  when is_integer(Value) ->
    {binary_to_list(Key), Value};

collection_member_to_list({Key, Value})
  when is_list(Value) ->
    {binary_to_list(Key), ?MODULE:collection_to_list(Value)};

collection_member_to_list({Key, <<>>}) ->
    {binary_to_list(Key), []};

collection_member_to_list({Key, Value})
  when is_binary(Value) ->
    {binary_to_list(Key), binary_to_list(Value)}.

collection_to_list(Collection) ->
    lists:map(fun({Key, Value}) ->
		      collection_member_to_list({Key, Value})
	      end,
	      Collection).
