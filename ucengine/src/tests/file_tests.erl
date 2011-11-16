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
-module(file_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

file_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([_, BaseUrl, [Root|_]]) ->
              [?_test(test_upload_small(BaseUrl, Root)),
               ?_test(test_upload_with_force_content_type(BaseUrl, Root)),
               ?_test(test_upload_big(BaseUrl, Root)),
               ?_test(test_upload_big_param(BaseUrl, Root)),
               ?_test(test_upload_not_found_meeting(BaseUrl, Root)),

               ?_test(test_list(BaseUrl, Root)),
               ?_test(test_list_reverse(BaseUrl, Root)),
               ?_test(test_list_not_found_meeting(BaseUrl, Root)),

               ?_test(test_get(BaseUrl, Root)),
               ?_test(test_get_not_found(BaseUrl, Root)),

               ?_test(test_delete(BaseUrl, Root))]
      end
    }.

gen_file(Size, FileName) ->
    Body = string:copies("content", Size),
    "------WebKitFormBoundaryLwCN5mZmxIA54Aif\r\n" ++
        "Content-Disposition: form-data; name=\"content\"; filename=\"" ++ FileName ++ "\"\r\n" ++
        "Content-Type: application/octet-stream\r\n\r\n" ++
        Body ++ "\r\n".

gen_param(Name, Value) ->
    "------WebKitFormBoundaryLwCN5mZmxIA54Aif\r\n" ++
        "Content-Disposition: form-data; name=\""++ Name ++"\"\r\n\r\n" ++
        Value ++ "\r\n".

gen_params(Body, []) ->
    Body ++ "------WebKitFormBoundaryLwCN5mZmxIA54Aif--\r\n";

gen_params(Body, [{Name, Value} | Rest]) ->
    gen_params(Body ++ gen_param(Name, Value), Rest).

gen_params(Params) when is_list(Params) ->
    gen_params("", Params).

upload(BaseUrl, Params, Body) ->
    upload(BaseUrl, "testmeeting", Params, Body).

upload(BaseUrl, Meeting, URIParams, Body) ->
    tests_utils:post(BaseUrl,
                     "/file/" ++ Meeting ++"/",
                     URIParams,
                     "multipart/form-data; boundary=----WebKitFormBoundaryLwCN5mZmxIA54Aif",
                     Body).

upload_raw(BaseUrl, Params, Body) ->
    upload_raw(BaseUrl, "testmeeting", Params, Body).
upload_raw(BaseUrl, Meeting, URIParams, Body) ->
    tests_utils:post_raw(BaseUrl,
                     "/file/" ++ Meeting ++"/",
                     URIParams,
                     "multipart/form-data; boundary=----WebKitFormBoundaryLwCN5mZmxIA54Aif",
                     Body).

test_upload_small(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    {struct,[{"result", _}]} = upload(BaseUrl, [], gen_file(4, "small.pdf") ++ gen_params(Params)),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"type", "internal.file.add"},
                 {"count", "1"},
                 {"order", "desc"}],
    {struct,[{"result", {array, [{struct, [{"type", "internal.file.add"},
                                           {"domain", _},
                                           {"datetime", _},
                                           {"id", _},
                                           {"location", "testmeeting"},
                                           {"from", RootUid},
                                           {"metadata", Metadata}]}]}}]} =
        tests_utils:get(BaseUrl, "/event/testmeeting", ParamsGet),
    ?assertMatch({struct, [{"id", _},
                           {"domain", _},
                           {"name", "small.pdf"},
                           {"size", "28"},
                           {"mime", "application/pdf"},
                           {"description", "test_file"}]}, Metadata).

test_upload_with_force_content_type(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"},
              {"forceContentType", "text/html"}],
    {ok, "201", Headers, _} = upload_raw(BaseUrl, [], gen_file(4, "small.pdf") ++ gen_params(Params)),
    ?assertEqual(lists:keyfind("Content-Type", 1, Headers), {"Content-Type", "text/html"}).

test_upload_big(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    ?assertMatch({struct,[{"result", _}]}, upload(BaseUrl, [], gen_file(4000, "big") ++ gen_params(Params))).

test_upload_big_param(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", string:copies("test_file", 4000)}],
    ?assertMatch({struct,[{"result", _}]}, upload(BaseUrl, [], gen_file(4, "small") ++ gen_params(Params))).

test_upload_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    ?assertEqual({struct,[{"error", "not_found"}]}, upload(BaseUrl, "nonexistentmeeting", [], gen_file(4, "small") ++ gen_params(Params))).

test_list(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    Result = tests_utils:get(BaseUrl, "/file/testmeeting/", Params),
    ?assertMatch({struct,
     [{"result",
       {array,
        [{struct,
          [{"id", _},
           {"domain", _},
           {"name",_},
           {"uri", _},
           {"location", "testmeeting"},
           {"metadata",{struct,[{"description", "test_file"}]}}]}|_]}}]}, Result).

test_list_reverse(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"order", "desc"}],
    Result = tests_utils:get(BaseUrl, "/file/testmeeting/", Params),
    String = string:copies("test_file", 4000),
    ?assertMatch({struct,
     [{"result",
       {array,
        [{struct,
          [{"id", _},
           {"domain", _},
           {"name",_},
           {"uri", _},
           {"location", "testmeeting"},
           {"metadata",{struct,[{"description", String}]}}]}|_]}}]}, Result).

test_list_not_found_meeting(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    ?assertMatch({struct,[{"error", "not_found"}]}, tests_utils:get(BaseUrl, "/file/unexistentmeeting/", Params)).

test_get(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    {struct,[{"result", Id}]} = upload(BaseUrl, [], gen_file(4, "small") ++ gen_params(Params)),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid}],
    tests_utils:get_raw(BaseUrl, "/file/testmeeting/" ++ Id, ParamsGet).

test_get_not_found(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/file/testmeeting/unexistentfile", Params).

test_delete(BaseUrl, {RootUid, RootSid}) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],

    {struct,[{"result", Id}]} = upload(BaseUrl, [], gen_file(4, "small") ++ gen_params(Params)),

    ?assertMatch({struct,[{"result", "ok"}]},
        tests_utils:delete(BaseUrl, "/file/testmeeting/" ++ Id, Params)),

    ?assertMatch({struct,[{"error", "not_found"}]},
        tests_utils:get(BaseUrl, "/file/testmeeting/" ++ Id, Params)).
