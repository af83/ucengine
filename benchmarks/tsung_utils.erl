-module(tsung_utils).

-export([extract_last_timestamp/1]).

extract_last_timestamp({_Pid, DynData}) ->
    {ok, Events} = ts_dynvars:lookup(events, DynData),
    if
        Events == undefined; Events == [] ->
            % Retry with the same timestamp
            case ts_dynvars:lookup(last, DynData) of
                {ok, LastTimestamp} ->
                    LastTimestamp;
            % Or the first timestamp
                false ->
                    0
            end;
        true ->
            {struct, EventAttributes} = lists:last(Events),
            {<<"datetime">>, Datetime} = lists:keyfind(<<"datetime">>, 1, EventAttributes),
            integer_to_list(Datetime)
    end.

