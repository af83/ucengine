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
-module(file_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

file_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([_, BaseUrl, Testers]) ->
	      [?_test(test_upload_small(BaseUrl, Testers)),
	       ?_test(test_upload_big(BaseUrl, Testers)),
	       ?_test(test_upload_not_found_meeting(BaseUrl, Testers)),

	       ?_test(test_list(BaseUrl, Testers)),
	       ?_test(test_list_not_found_meeting(BaseUrl, Testers)),

	       ?_test(test_get(BaseUrl, Testers)),
	       ?_test(test_get_not_found(BaseUrl, Testers)),

	       ?_test(test_delete(BaseUrl, Testers))]
      end
    }.

gen_file(Size, FileName) ->
    Body = string:copies("content", Size),
    "------WebKitFormBoundaryLwCN5mZmxIA54Aif\r\n" ++
	"Content-Disposition: form-data; name=\"content\"; filename=\"" ++ FileName ++ "\"\r\n" ++ 
	"Content-Type: application/octet-stream\r\n\r\n" ++
	Body ++ "\r\n" ++
	"------WebKitFormBoundaryLwCN5mZmxIA54Aif--\r\n".

upload(BaseUrl, Params, File) ->
    upload(BaseUrl, "testmeeting", Params, File).

upload(BaseUrl, Meeting, Params, File) ->
    tests_utils:post(BaseUrl,
                     "/file/" ++ Meeting ++"/", 
                     Params,
                     "multipart/form-data; boundary=----WebKitFormBoundaryLwCN5mZmxIA54Aif",
                     File).

test_upload_small(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    {struct,[{"result", _}]} = upload(BaseUrl, Params, gen_file(4, "small")),
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
    {struct, [{"id", _},
              {"domain", _},
              {"name", "small"}, 
              {"size", "28"}, 
              {"mime", "text/plain"}]} = Metadata.

test_upload_big(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    {struct,[{"result", _}]} = upload(BaseUrl, Params, gen_file(4000, "big")).

test_upload_not_found_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"metadata[description]", "test_file"}],
    {struct,[{"error", "not_found"}]} = upload(BaseUrl, "testorg", Params, gen_file(4, "small")).

test_list(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct,
     [{"result",
       {array,
        [{struct,
          [{"id", _},
           {"domain", _},
           {"name",_},
           {"uri", _},
           {"location", "testmeeting"},
           {"metadata",{struct,_}}]}|_]}}]} = tests_utils:get(BaseUrl, "/file/testmeeting/", Params).

test_list_not_found_meeting(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get(BaseUrl, "/file/unexistentmeeting/", Params).

test_get(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid},
              {"metadata[description]", "test_file"}],
    {struct,[{"result", Id}]} = upload(BaseUrl, Params, gen_file(4, "small")),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid}],
    tests_utils:get_raw(BaseUrl, "/file/testmeeting/" ++ Id, ParamsGet),
    true.

test_get_not_found(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/file/testmeeting/unexistentfile", Params).

test_delete(BaseUrl, [{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
              {"sid", RootSid}],
    {struct,[{"result", Id}]} =
        upload(BaseUrl, Params, gen_file(4, "small")),    ParamsDelete = [{"uid", RootUid},
                                                                          {"sid", RootSid}],
    {struct,[{"result", "ok"}]} =
        tests_utils:delete(BaseUrl, "/file/testmeeting/" ++ Id, ParamsDelete),
    
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} =
        tests_utils:get(BaseUrl, "/file/testmeeting/" ++ Id, ParamsGet).
