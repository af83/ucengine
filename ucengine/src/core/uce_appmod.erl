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
-module(uce_appmod).

-author('victor.goya@af83.com').

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1]).

convert2(string, Value) ->
    Value;
convert2(integer, Value) when is_integer(Value) ->
    Value;
convert2(integer, Value) ->
    case string:to_integer(Value) of
        {error, _} ->
            {error, bad_parameters};
        {Integer, _} ->
            Integer
    end;
convert2(atom, Value) when is_atom(Value) ->
    Value;
convert2(atom, Value) ->
    list_to_atom(Value);
convert2(dictionary, Value) ->
    Value;
convert2(file, Value) when is_record(Value, file_upload) ->
    Value;
convert2(file, _Value) ->
    {error, bad_parameters}.

convert(Param, Type)
  when is_atom(Type) ->
    convert(Param, [Type]);
convert(_, []) ->
    throw({error, bad_parameters});
convert(Param, [Type|Tail]) ->
    Result = convert2(Type, Param),
    case Result of
        {error, _} ->
            convert(Param, Tail);
        _ ->
            Result
    end.

validate(_, []) ->
    [];
validate(Query, [{Name, Default, Types}|ParamsSpecList]) ->
    case utils:get(Query, [Name], [Default]) of
        [required] ->
            throw({error, missing_parameters, lists:flatten(io_lib:format("Parameter '~s' is missing", [Name]))});
        [RawValue] ->
            [convert(RawValue, Types)] ++ validate(Query, ParamsSpecList)
    end.

call_handlers(Domain, {Module, Function, ParamsSpecList}, Query, Match, Arg) ->
    case catch validate(Query, ParamsSpecList) of
        {error, Reason} ->
            json_helpers:error(Domain, Reason);
        {error, Reason, Infos} ->
            ?ERROR_MSG("~p: error: ~p:~p: ~p ~p~n", [Domain, Module, Function, Reason, Infos]),
            json_helpers:error(Domain, Reason, Infos);
        Params ->
            ?DEBUG("~p: call ~p:~p matching ~p with ~p~n", [Domain, Module, Function, Match, Params]),
            Now = now(),
            try Module:Function(Domain, Match, Params, Arg) of
                {streamcontent_with_timeout, _, _, _} = Stream ->
                    cors_helpers:format_cors_headers(Domain) ++ [Stream];
                Response when is_list(Response) ->
                    ?TIMER_APPEND(atom_to_list(Module) ++ "_" ++ atom_to_list(Function), Now),
                    Response
            catch
                {error, Reason} ->
                    ?ERROR_MSG("~p: error: ~p:~p: ~p ~p~n", [Domain, Module, Function, Reason, Params]),
                    json_helpers:error(Domain, Reason);
                E ->
                    ?ERROR_MSG("~p: error: ~p:~p: ~p ~p~n", [Domain, Module, Function, Params, E]),
                    json_helpers:unexpected_error(Domain)
            end
    end.

%%
%% Function called by yaws
%% For each vhost we support, we store to the opaque field the current domain
%%
out(#arg{} = Arg) ->
    ?COUNTER('http:request'),
    Host = Arg#arg.opaque,
    case uce_http:parse(Host, Arg) of
        {error, Reason} ->
            json_helpers:error(Host, Reason);
        {get_more, _, _} = State ->
            State;
        {Method, Path, Query} ->
            process(Host, Method, Path, Query, Arg)
    end.

% Handle options method. If one route match, return ok
process(Host, 'OPTIONS', Path, _Query, _Arg) ->
    case routes:get(Path) of
        {ok, _Match, _Handlers} ->
            json_helpers:ok(Host);
        {error, not_found} ->
            ?ERROR_MSG("options ~p: no route found", [Path]),
            json_helpers:error(Host, not_found)
    end;
% Handle head method. If a 'GET' route match, normal process
% Yaws will strip the content
process(Host, 'HEAD', Path, Query, Arg) ->
    process(Host, 'GET', Path, Query, Arg);
process(Host, Method, Path, Query, Arg) ->
    ContentType = Arg#arg.headers#headers.content_type,
    case routes:get(Method, Path, ContentType) of
        {ok, Match, Handlers} ->
            call_handlers(Host, Handlers, Query, Match, Arg);
        {error, not_found} ->
            ?ERROR_MSG("~p ~p: no route found", [Method, Path]),
            json_helpers:error(Host, not_found)
    end.
