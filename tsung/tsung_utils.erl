-module(tsung_utils).

-export([extract_last_timestamp/1]).

extract_last_timestamp({_Pid, DynData}) ->
    {ok, Events} = ts_dynvars:lookup(events, DynData),
    case Events of
    undefined ->
        % An error has occured, retry with the same timestamp
        case ts_dynvars:lookup(last, DynData) of
        {ok, LastTimestamp} ->
            LastTimestamp;
        % Or the first timestamp
        false ->
            0
        end;
    Events when is_list(Events) ->
        {struct, EventAttributes} = lists:last(Events),
        {<<"datetime">>, Datetime} = lists:keyfind(<<"datetime">>, 1, EventAttributes),
        integer_to_list(Datetime)
    end.

