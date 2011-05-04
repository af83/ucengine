-module(uce_log_event).
-author('Mathieu Lecarme mathieu.lecarme@af83.com').

-export([init/1, terminate/1, handle_event/2]).

init(_InitData) ->
    ok.
terminate(_Data) ->
    ok.
handle_event({error, Gleader, {Pid, Format, Data}}, _Data2) ->
    uce_logger:log(4, error_logger, 0, "[~w ~w] " ++ Format, [Gleader, Pid] ++ Data);
handle_event({error_report, Gleader, {Pid, std_error, Report}}, _Data) ->
    uce_logger:log(4, error_logger, 0, "[~w ~w] ~s" , [Gleader, Pid, fmt_report(Report)]).
% handle_event({error_report, Gleader, {Pid, Type, Report}}, Data) ->
%     ok;
% handle_event({warning_msg, Gleader, {Pid, Format, Data}}, Data) ->
%     ok;
% handle_event({warning_report, Gleader, {Pid, std_warning, Report}}, Data) ->
%     ok;
% handle_event({warning_report, Gleader, {Pid, Type, Report}}, Data) ->
%     ok;
% handle_event({info_msg, Gleader, {Pid, Format, Data}}, Data) ->
%     ok;
% handle_event({info_report, Gleader, {Pid, std_info, Report}}, Data) ->
%     ok;
% handle_event({info_report, Gleader, {Pid, Type, Report}}, Data) ->
%     ok.



fmt_report(Report) when is_list(Report)->
    list:flatten(list:map(
        fun ({Tag, Data}) -> io_lib:format("~w : ~w", [Tag, Data]);
            (A) -> A
        end
    , Report));
fmt_report(Report) ->
    Report.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
    fmt_report_test() ->
        ok.
-endif.    