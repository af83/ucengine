-module(file_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

file_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun(Testers) ->
	      [?_test(test_upload_small(Testers)),
	       ?_test(test_upload_big(Testers)),
	       ?_test(test_upload_not_found_org(Testers)),
	       ?_test(test_upload_not_found_meeting(Testers)),

	       ?_test(test_list(Testers)),
	       ?_test(test_list_not_found_org(Testers)),
	       ?_test(test_list_not_found_meeting(Testers)),

	       ?_test(test_get(Testers)),
	       ?_test(test_get_not_found(Testers)),

	       ?_test(test_delete(Testers))]
      end
    }.

gen_file(Size, FileName) ->
    Body = string:copies("content", Size),
    "------WebKitFormBoundaryLwCN5mZmxIA54Aif\r\n" ++
	"Content-Disposition: form-data; name=\"content\"; filename=\"" ++ FileName ++ "\"\r\n" ++ 
	"Content-Type: application/octet-stream\r\n\r\n" ++
	Body ++ "\r\n" ++
	"------WebKitFormBoundaryLwCN5mZmxIA54Aif--\r\n".

upload(Params, File) ->
    upload("testmeeting", Params, File).

upload(Meeting, Params, File) ->
    upload("testorg", Meeting, Params, File).

upload(Org, Meeting, Params, File) ->
    tests_utils:post("/file/"++ Org ++"/" ++ Meeting ++"/", 
                     Params,
                     "multipart/form-data; boundary=----WebKitFormBoundaryLwCN5mZmxIA54Aif",
                     File).

test_upload_small([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"}, 
	      {"metadata[description]", "test_file"}],
    {struct,[{"result", _}]} = upload(Params, gen_file(4, "small")),
    ParamsGet = [{"uid", RootUid},
                 {"sid", RootSid},
                 {"type", "internal.file.add"},
                 {"count", "1"},
                 {"order", "desc"}],
    {struct,[{"result", {array, [{struct, [{"type", "internal.file.add"},
			 {"datetime", _},
			 {"id", _},
			 {"org", "testorg"},
			 {"meeting", "testmeeting"},
			 {"from", RootUid}, 
			 {"metadata", Metadata}]}]}}]} = tests_utils:get("/event/testorg/testmeeting", ParamsGet),
    {struct, [{"id", _},
              {"name", "small"}, 
              {"size", "28"}, 
              {"mime", "text/plain"}]} = Metadata.

test_upload_big([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"},
	      {"metadata[description]", "test_file"}],
    {struct,[{"result", _}]} = upload(Params, gen_file(4000, "big")).

test_upload_not_found_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"},
	      {"metadata[description]", "test_file"}],
    {struct,[{"error", "not_found"}]} = upload("unexistentorg", "testmeeting", Params, gen_file(4, "small")).

test_upload_not_found_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"},
	      {"metadata[description]", "test_file"}],
    {struct,[{"error", "not_found"}]} = upload("testorg", "unexistentmeeting", Params, gen_file(4, "small")).

test_list([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,
     [{"result",
       {array,
	[{struct,
	  [{"id", _},
	   {"name",_},
	   {"uri", _},
	   {"org", "testorg"},
	   {"meeting", "testmeeting"},
	   {"metadata",{struct,_}}]}|_]}}]} = tests_utils:get("/file/testorg/testmeeting/", Params).

test_list_not_found_org([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/file/unexistentorg/testmeeting/", Params).

test_list_not_found_meeting([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/file/testorg/unexistentmeeting/", Params).

test_get([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"},
	      {"metadata[description]", "test_file"}],
    {struct,[{"result", Id}]} = upload(Params, gen_file(4, "small")),
    ParamsGet = [{"uid", RootUid},
		 {"sid", RootSid}],
    tests_utils:get_raw("/file/testorg/testmeeting/" ++ Id, ParamsGet),
    true.

test_get_not_found([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/file/testorg/testmeeting/unexistentfile", Params).

test_delete([{RootUid, RootSid}, _]) ->
    Params = [{"uid", RootUid},
	      {"sid", RootSid},
	      {"_method", "put"}],
    {struct,[{"result", Id}]} = upload(Params, gen_file(4, "small")),

    ParamsDelete = [{"uid", RootUid},
		    {"sid", RootSid}],
    {struct,[{"result", "ok"}]} = tests_utils:delete("/file/testorg/testmeeting/" ++ Id, ParamsDelete),
    
    ParamsGet = [{"uid", RootUid},
		 {"sid", RootSid}],
    {struct,[{"error", "not_found"}]} = tests_utils:get("/file/testorg/testmeeting/" ++ Id, ParamsGet).

