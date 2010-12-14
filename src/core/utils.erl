-module(utils).

-author('tbomandouki@af83.com').

-include("uce.hrl").

-export([
	 now/0,
	 token/0,
	 uid/0,

	 random/0,
	 random/1,
	 get/2,
	 get/3,
%	 timestamp_to_ical_date/1

	 format/1
	]).

%% Get current timestamp
now() ->
    {Mega,Sec,Micro} = erlang:now(),
    erlang:round(((Mega*1000000+Sec)*1000000+Micro)/1000).

token() ->
    ?MODULE:random().

uid() ->
    "uid_" ++ ?MODULE:random().

random() ->
    ?MODULE:random(32).
random(0) ->
    [];
random(Length) ->
    [crypto:rand_uniform(48,58)] ++ ?MODULE:random(Length - 1).

get(Params, Key) when is_atom(Key) ->
    [Result] = get(Params, [Key]),
    Result;
get(Params, Keys) ->
    get(Params, Keys, none).
get(Params, Keys, Default) when is_atom(Default) ->
    get(Params, Keys, lists:map(fun(_Elem) ->
					Default
				end,
				Keys));
get(_Params, [], []) ->
    [];
get(Params, [Key|Keys], [Default|Defaults]) ->
    ValueList = case lists:keysearch(Key, 1, Params) of
		    {value, {Key, Value}} ->
			[Value];
		    false ->
			[Default]
		end,
    ValueList ++ ?MODULE:get(Params, Keys, Defaults).

% 19970714T170000Z
%% timestamp_to_ical_date(Date) when is_integer(Date) ->
%% 	if
%% 		Date > 0 ->
%% 			BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
%% 			{{Y,M,D},{H,Min,S}} = calendar:gregorian_seconds_to_datetime(BaseDate + (Date div 1000) ),
%% 			int2str(Y) ++ int2str(M) ++ int2str(D) ++ "T" ++ int2str(H) ++ int2str(Min) ++ int2str(S) ++ "Z";
%% 		true -> none
%% 	end;
%% timestamp_to_ical_date(_) -> "".

format(R) when is_atom(R) -> atom_to_list(R);
format(Tuple) when is_tuple(Tuple) ->
	HdList = tuple_to_list(Tuple),
	RecName = hd(HdList),
	RecFieldsValue = tl(HdList),
	RecFields = mnesia:table_info(RecName, attributes),
	FieldsStr = format_fields(RecFields, RecFieldsValue),
	lists:flatten(atom_to_list(RecName) ++ "~n" ++ FieldsStr);
format([HdTuple | TlTuple]) when is_tuple(HdTuple) ->
	lists:flatten(format(HdTuple) ++ format(TlTuple));
format([]) -> "[]";
format(_) -> "Unknown returned value".	

format_fields([], []) -> "";
format_fields([Field | Fields], [Value | Values]) when Field == location ; Field == roster ->
	[ "metadata["++Key++"] = " ++ string:join(Val, ", ") ++ "~n" || {Key, Val} <- Value ] ++ format_fields(Fields, Values);	
format_fields([metadata | Fields], [Value | Values]) when is_list(Value) ->
	[ "metadata["++Key++"] = " ++ Val ++ "~n" || {Key, Val} <- Value ] ++ format_fields(Fields, Values);	
format_fields([Field | Fields], [Value | Values]) when is_integer(Value) ->
	atom_to_list(Field) ++ " = " ++ integer_to_list(Value) ++ "~n" ++ format_fields(Fields, Values);
format_fields([Field | Fields], [Value | Values]) when is_atom(Value) ->
	atom_to_list(Field) ++ " = " ++ atom_to_list(Value) ++ "~n" ++ format_fields(Fields, Values);
format_fields([Field | Fields], [Value | Values]) when is_list(Value) ->
	atom_to_list(Field) ++ " = " ++ Value ++ "~n" ++ format_fields(Fields, Values).
