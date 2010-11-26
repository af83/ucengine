-module(file_controller).

-export([init/0, add/3, list/3, get/3, delete/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Files",
		method='GET',
		regexp="/file/([^/]+)/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, list,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Files",
		method='GET',
		regexp="/file/([^/]+)/([^/]+)/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, get,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Files",
		method='PUT',
		regexp="/file/([^/]+)/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "_filename", "_uri", "metadata"],
			    [required, required, required, []],
			    [string, string, string, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Files",
		method='DELETE',
		regexp="/file/([^/]+)/([^/]+)/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, delete,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]}].

add(Location, [EUid, Name, Uri, Metadata], _) ->
    case uce_acl:check(EUid, "file", "add", Location, []) of
	true ->
	    case uce_file:add(#uce_file{location=Location,
					name=Name,
					uri=Uri,
					metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		Id ->
		    uce_event:add(#uce_event{location=Location,
					     from=EUid,
					     type="internal.file.add",
					     metadata=[{"id", Id}]}),
		    file_helpers:upload(Id)
	    end;
	false ->
	    {error, unauthorized}
    end.

list(Location, [EUid], _) ->
    case uce_acl:check(EUid, "file", "list", Location, []) of
	true ->		    
	    case uce_file:list(Location) of
		{error, Reason} ->
		    {error, Reason};
		Files ->
		    json_helpers:json({array, [file_helpers:to_json(File) || File <- Files]})
	    end;
	false ->
	    {error, unauthorized}
    end.

% from YAWS
sanitize_file_name(".." ++ T) ->
    sanitize_file_name([$.|T]);
sanitize_file_name([H|T]) ->
    case lists:member(H,  " &;'`{}!\\?<>\"()$ ?") of
        true ->
            sanitize_file_name(T);
        false ->
            [H|sanitize_file_name(T)]
    end;
sanitize_file_name([]) ->
    [].

get([Org, Meeting, Id], [EUid], _) ->
    case uce_acl:check(EUid, "file", "get", [Org, Meeting], [{"id", Id}]) of
	true ->
	    case uce_file:get(Id) of
		{error, Reason} ->
		    {error, Reason};
		File ->
		    case re:run(File#uce_file.uri, "^([^/]+)://(.+)", [{capture, all, list}]) of
			{match, [_, "file", UnsafePath]} ->
			    Path = config:get(datas) ++ "/" ++ sanitize_file_name(UnsafePath),
			    case file:read_file(Path) of
				{error, Reason} ->
				    {error, Reason};
				{ok, Content} ->
				    file_helpers:download(File#uce_file.id, Content)
			    end;
			_ ->
			    {error, not_implemented}
		    end
	    end;
	false ->
	    {error, unauthorized}
    end.

delete([Org, Meeting, Id], [EUid], _) ->
    case uce_acl:check(EUid, "file", "delete", [Org, Meeting], [{"id", Id}]) of
	true ->
	    case uce_file:delete(Id) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.
