%%
%%  U.C.Engine - Unified Colloboration Engine
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
-module(appmod_uce).

-author('victor.goya@af83.com').

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1, call_handlers/4]).

convert(Param, Type)
  when is_atom(Type) ->
    convert(Param, [Type]);
convert(_, []) ->
    throw({error, bad_parameters});
convert(Param, [Type|Tail]) ->
    Result = if
                 Type == string ->
                     Param;
                 Type == integer,
                 is_integer(Param) ->
                     Param;
                 Type == integer,
                 is_integer(Param) == false ->
                     case string:to_integer(Param) of
                         {error, _} ->
                             {error, bad_parameters};
                         {Integer, _} ->
                             Integer
                     end;
                 Type == atom,
                 is_atom(Param)->
                     Param;
                 Type == atom,
                 is_atom(Param) == false ->
                     case catch list_to_atom(Param) of
                         Atom when is_atom(Atom) ->
                             Atom;
                         _ ->
                             {error, bad_parameters}
                     end;
                 Type == dictionary ->
                     Param
             end,
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
        {error, Reason} ->
            throw({error, Reason});
        [required] ->
            throw({error, missing_parameters});
        [RawValue] ->
            [convert(RawValue, Types)] ++ validate(Query, ParamsSpecList)
    end.

call_handlers({Module, Function, ParamsSpecList}, Query, Match, Arg) ->
    case catch validate(Query, ParamsSpecList) of
        {error, Reason} ->
            json_helpers:error(Reason);
        Params ->
            ?DEBUG("~p~n", [Params]),
            Domain =
                case catch string:sub_word(Arg#arg.headers#headers.host, 1, $:) of
                    Result when is_list(Result) ->
                        Result;
                    _ ->
                        config:get(default_domain)
                end,

            ?DEBUG("~p: call ~p:~p matching ~p with ~p~n", [Domain, Module, Function, Match, Params]),
            case catch Module:Function(Domain, Match, Params, Arg) of
                {error, Reason} ->
                    ?ERROR_MSG("~p: error: ~p:~p: ~p~n", [Domain, Module, Function, Reason]),
                    json_helpers:error(Reason);
                {'EXIT', {function_clause, [{Module, Function,_}|_]}} ->
                    ?ERROR_MSG("~p: error: ~p:~p: function not found~n", [Domain, Module, Function]),
                    json_helpers:error(not_found);
                {'EXIT', Reason} ->
                    ?ERROR_MSG("~p: error: ~p:~p: ~p~n", [Domain, Module, Function, Reason]),
                    json_helpers:error(Reason);
                {streamcontent_from_pid, _, _} = Stream ->
                    Stream;
                Response when is_list(Response) ->
                    Response;
                Error ->
                    ?ERROR_MSG("~p: error: ~p:~p: ~p~n", [Domain, Module, Function, Error]),
                    json_helpers:unexpected_error()
            end
    end.

out(#arg{} = Arg) ->
    case uce_http:parse(Arg) of
        {error, Reason} ->
            json_helpers:error(Reason);
        {get_more, _, _} = State ->
            State;  
      {Method, Path, Query} ->
            process(Method, Path, Query, Arg);
        _ ->
            json_helpers:unexpected_error()
    end.

process(Method, Path, Query, Arg) ->
    case routes:get(Method, Path) of
        {ok, Match, Handlers} ->
            ?MODULE:call_handlers(Handlers, Query, Match, Arg);
        {error, not_found} ->
            ?ERROR_MSG("~p ~p: no route found~n", [Method, Path]),
            json_helpers:error(not_found);
        {error, Reason} ->
            json_helpers:error(Reason)
    end.
