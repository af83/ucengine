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
-module(uce_middleware_parse).

-export([call/2]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

write_file(Data, File) ->
    case file:write(File#file_upload.fd, Data) of
        ok ->
            ok;
        Err ->
            ?ERROR_MSG("Upload error: ~p.~n", [Err]),
            error
    end.

process_part(_Host, _Arg, [], Params) ->
    Params;
process_part(_Host, _Arg, [{part_body, Data}|_Res], [{_Name, File}|_] = Params) when is_record(File, file_upload) ->
    case write_file(Data, File) of
        ok ->
            Params;
        error ->
            {error, unexpected_error}
    end;
process_part(_Host, _Arg, [{part_body, Data}|_Res], [{Name, Value}|OtherParams]) ->
    [{Name, Value ++ Data}] ++ OtherParams;
process_part(Host, Arg, [{body, Data}|Res], [{_Name, File}|_] = Params) when is_record(File, file_upload) ->
    case write_file(Data, File) of
        ok ->
            ok = file:close(File#file_upload.fd),
            process_part(Host, Arg, Res, Params);
        error ->
            {error, unexpected_error}
    end;
process_part(Host, Arg, [{body, Data}|Rest], [{Name, Value}|OtherParams]) ->
    process_part(Host, Arg, Rest, [{Name, Value ++ Data}] ++ OtherParams);
process_part(Host, Arg, [{head, {Name, Opts}}|Res], Params) ->
    case lists:keyfind(filename, 1, Opts) of
        {_, Fname} ->
            Dir = lists:concat([config:get(Host, data), "/", utils:random(3)]),
            FilePath = lists:concat([Dir, "/", utils:random()]),
            file:make_dir(Dir),
            case file:open(FilePath,[write]) of
                {ok, Fd} ->
                    File = #file_upload{filename = Fname,
                                        uri = "file://"++ FilePath,
                                        fd = Fd},
                    process_part(Host, Arg, Res, [{Name, File}] ++ Params);
                Err ->
                    ?ERROR_MSG("Upload error: ~p.", [Err]),
                    {error, unexpected_error}
            end;
        false ->
            process_part(Host, Arg, Res, [{Name, ""}] ++ Params)
    end.

parse_multipart(Host, Arg, State) ->
    case yaws_api:parse_multipart_post(Arg) of
        {cont, Cont, Res} ->
            NewState = process_part(Host, Arg, Res, State),
            case NewState of
                {error, _Error} ->
                    NewState;
                NewState ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            FormDataParams = process_part(Host, Arg, Res, State),
            case FormDataParams of
                {error, _Error} ->
                    FormDataParams;
                FormDataParams ->
                    Params = yaws_api:parse_query(Arg) ++ FormDataParams,
                    {ok, parse_query(Params)}
            end
    end.

extract(Host, Arg, State) ->
    Request = Arg#arg.req,
    case Request#http_request.method of
        'GET' ->
            {ok, parse_query(yaws_api:parse_query(Arg))};
        _ ->
            case Arg#arg.headers#headers.content_type of
                "multipart/form-data;"++ _Boundary ->
                    parse_multipart(Host, Arg, State);
                "application/x-www-form-urlencoded" ++ _Charset ->
                    NewArg = Arg#arg{req = Arg#arg.req#http_request{method = 'POST'}},
                    Query = yaws_api:parse_post(NewArg) ++ yaws_api:parse_query(NewArg),
                    {ok, parse_query(Query)};
                _ ->
                    Query = yaws_api:parse_query(Arg),
                    {ok, parse_query(Query)}
            end
    end.

parse(Host, #arg{state=undefined} = Arg) ->
    extract(Host, Arg, []);
parse(Host, #arg{} = Arg) ->
    extract(Host, Arg, Arg#arg.state).

extract_dictionary([], _) ->
    [];
extract_dictionary([{DictElem, Value}|Tail], Name) ->
    Elem = case re:run(DictElem, "^(.*)\\[([^\]]+)\\]$ ?", [{capture, all, list}]) of
               {match, [_, Name, Key]} ->
                   [{Key, Value}];
               _ ->
                   []
           end,
    Elem ++ extract_dictionary(Tail, Name).

remove_key([], _) ->
    [];
remove_key([{Key, Value}|Tail], Name) ->
    ElemName = case re:run(Key, "^(.*)\\[([^\]]+)\\]$ ?", [{capture, all, list}]) of
                   {match, [_, Name, _]} ->
                       Name;
                   _ ->
                       Key
               end,
    Elem = case ElemName of
               Name ->
                   [];
               _ ->
                   [{Key, Value}]
           end,
    Elem ++ remove_key(Tail, Name).

parse_query_elems([]) ->
    [];
parse_query_elems([{Key, Value}|Tail]=Query) ->
    Elem = case re:run(Key, "^(.*)\\[([^\]]+)\\]$ ?", [{capture, all, list}]) of
               {match, [_, Name, _]} ->
                   [{Name, extract_dictionary(Query, Name)}];
               _ ->
                   [{Key, Value}]
           end,
    [{ToRemove, _}] = Elem,
    Elem ++ parse_query_elems(remove_key(Tail, ToRemove)).

parse_query(AsciiDirtyQuery) ->
    AsciiQuery = lists:filter(fun({Key, _}) ->
                                      case Key of
                                          [] ->
                                              false;
                                          _ ->
                                              true
                                      end
                              end,
                              AsciiDirtyQuery),
    Query = lists:map(fun({Key, Value}) ->
                              case Value of
                                  undefined ->
                                      {unicode_helpers:normalize_unicode(Key), ""};
                                  Value when is_record(Value, file_upload) ->
                                      FileName = Value#file_upload.filename,
                                      {unicode_helpers:normalize_unicode(Key), Value#file_upload{
                                        filename=unicode_helpers:normalize_unicode(FileName)
                                     }};
                                  _ ->
                                      {unicode_helpers:normalize_unicode(Key),
                                       unicode_helpers:normalize_unicode(Value)}
                              end
                      end,
                      AsciiQuery),
    parse_query_elems(Query).

%%
%% Parse http query and body
%%
-spec call(Request :: request(), Response :: response()) -> {ok, request(), response()} | {stop, response()}.
call(#uce_request{domain=Domain, arg=Arg} = Request, Response) ->
    case parse(Domain, Arg) of
        {error, Reason} ->
            {stop, json_helpers:error(Response, Reason)};
        {get_more, _, _} = State ->
            State;
        {ok, Query} ->
            {ok, Request#uce_request{qparams=Query}, Response}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_query_test() ->
    ?assertEqual([{"Test", "test"}], parse_query([{"Test", "test"}])),
    ?assertEqual([{"test", [{"to", "test"}]}], parse_query([{"test[to]", "test"}])).

-endif.
