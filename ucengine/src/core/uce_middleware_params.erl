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
-module(uce_middleware_params).

-export([call/3]).

-include("uce.hrl").

convert(string, Value) ->
    Value;
convert(integer, Value) when is_integer(Value) ->
    Value;
convert(integer, Value) ->
    case string:to_integer(Value) of
        {error, _} ->
            {error, bad_parameters};
        {Integer, _} ->
            Integer
    end;
convert(atom, Value) when is_atom(Value) ->
    Value;
convert(atom, Value) ->
    list_to_atom(Value);
convert(dictionary, Value) ->
    Value;
convert(file, Value) when is_record(Value, file_upload) ->
    Value;
convert(file, _Value) ->
    {error, bad_parameters}.

validate(_, [], Acc) ->
    Acc;
validate(Query, [{Name, Default, Type}|ParamsSpecList], Acc) ->
    case {Default, proplists:lookup(Name, Query)} of
        {required, none} ->
            {error, missing_parameters, lists:flatten(io_lib:format("Parameter '~s' is missing", [Name]))};
        {Default, none} ->
            validate(Query, ParamsSpecList, Acc ++ [Default]);
        {_Default, {Name, RawValue}} ->
            case convert(Type, RawValue) of
                {error, bad_parameters} ->
                    {error, bad_parameters, lists:flatten(io_lib:format("Parameter '~s' is not correct", [Name]))};
                Value ->
                    validate(Query, ParamsSpecList, Acc ++ [Value])
            end
    end.

%%
%% Extract params from the query
%%
-spec call(Request :: request(), Response :: response(), Params :: list({string(), atom()|string(), atom()})) -> {ok, request()} | {error, atom(), string}.
call(#uce_request{qparams=Query, params=Params} = Request, Response, ParamsSpecList) ->
    case validate(Query, ParamsSpecList, []) of
        {error, R1, R2} ->
            {stop, json_helpers:error(Response, R1, R2)};
        Params2 ->
            {ok, Request#uce_request{params=Params ++ Params2}, Response}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

convert_test() ->
    ?assertMatch("d", convert(string, "d")),

    ?assertMatch(0, convert(integer, 0)),
    ?assertMatch(10, convert(integer, "10")),
    ?assertMatch({error, bad_parameters}, convert(integer, "a")),
    %% FIXME: this test is not correct, should assert something like [{"", ""}, ..]
    ?assertMatch("a", convert(dictionary, "a")).

params_test() ->
    R = call(#uce_request{params=[], qparams=[]}, #uce_response{}, [{"type", required, string}]),
    ?assertMatch({stop, #uce_response{status=400}}, R),

    {ok, R2, _Resp1} = call(#uce_request{params=[], qparams=[{"type", "roger"}]}, #uce_response{}, [{"type", required, string}]),
    ?assertMatch(["roger"], R2#uce_request.params),

    {ok, R3, _Resp2} = call(#uce_request{params=[], qparams=[]}, #uce_response{}, [{"type", "hello", string}]),
    ?assertMatch(["hello"], R3#uce_request.params),

    {ok, R4, _Resp3} = call(#uce_request{params=[], qparams=[{"type", "10"}]}, #uce_response{}, [{"type", required, integer}]),
    ?assertMatch([10], R4#uce_request.params),

    {stop, #uce_response{status=400}} = call(#uce_request{params=[], qparams=[{"type", "e10"}]}, #uce_response{}, [{"type", required, integer}]),

    R5 = call(#uce_request{params=[], qparams=[{"type", "roger"}]}, #uce_response{}, [{"type", required, string}, {"test", required, string}]),
    ?assertMatch({stop, #uce_response{}}, R5),

    {ok, R6, _Resp4} = call(#uce_request{params=["roger"], qparams=[{"type", "10"}]}, #uce_response{}, [{"type", required, integer}]),
    ?assertMatch(["roger", 10], R6#uce_request.params),

    {ok, R7, _Resp5} = call(#uce_request{params=["roger"], qparams=[{"type", "10"}]}, #uce_response{}, [{"type", required, integer}, {"number", 3, integer}]),
    ?assertMatch(["roger", 10, 3], R7#uce_request.params).

-endif.
