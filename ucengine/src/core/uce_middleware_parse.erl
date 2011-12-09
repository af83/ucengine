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

extract(_Host, #uce_request{method=Method, arg=Req2}, _State) ->
    case Method of
        'GET' ->
            {ok, parse_query(misultin_req:parse_qs(Req2))};
        'HEAD' ->
            {ok, parse_query(misultin_req:parse_qs(Req2))};
        'OPTIONS' ->
            {ok, parse_query(misultin_req:parse_qs(Req2))};
        _ ->
            Query = misultin_req:parse_post(Req2) ++ misultin_req:parse_qs(Req2),
            {ok, parse_query(Query)}
    end.

parse(Host, Request) ->
    extract(Host, Request, []).

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
call(#uce_request{domain=Domain} = Request, Response) ->
    case parse(Domain, Request) of
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
