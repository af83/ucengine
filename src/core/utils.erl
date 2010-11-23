-module(utils).

-author('tbomandouki@af83.com').

-export([
	 now/0,
	 token/0,
	 uid/0,

	 random/0,
	 random/1,
	 get/2,
	 get/3
%	 timestamp_to_ical_date/1
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

