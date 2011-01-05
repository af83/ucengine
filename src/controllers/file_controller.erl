-module(file_controller).

-export([init/0, add/3, list/3, get/3, delete/3]).

-include("uce.hrl").

-include_lib("kernel/include/file.hrl").

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
	{ok, true} ->
	    case uce_file:add(#uce_file{location=Location,
					name=Name,
					uri=Uri,
					metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Id} ->
		    % XXX: shall the model returns this precious record of her ?
		    case uce_file:get(Id) of
			{ok, #uce_file{} = File} ->
                {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
			    uce_event:add(#uce_event{location=Location,
						     from=EUid,
						     type="internal.file.add",
						     metadata=[ {"id", File#uce_file.id},
								{"name", File#uce_file.name},
								{"size", integer_to_list(FileInfo#file_info.size)},
								{"mime", File#uce_file.mime}]}),
			    file_helpers:upload(File#uce_file.id);
			{error, Reason} ->
			    {error, Reason}
		    end
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

list(Location, [EUid], _) ->
    case uce_acl:check(EUid, "file", "list", Location, []) of
	{ok, true} ->
	    case uce_file:list(Location) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Files} ->
		    json_helpers:json({array, [file_helpers:to_json(File) || File <- Files]})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

%%
%% @doc Get real path from encoded uri of record uce_file
%% @spec (Uri::list) -> list
%%
get_path(Uri) ->
    re:replace(Uri, "file\:\/", config:get(datas), [{return, list}]).

get([Org, Meeting, Id], [EUid], _) ->
    case uce_acl:check(EUid, "file", "get", [Org, Meeting], [{"id", Id}]) of
	{ok, true} ->
	    case uce_file:get(Id) of
		{error, Reason} ->
		    {error, Reason};
		{ok, File} ->
		    Path = get_path(File#uce_file.uri),
			case file:read_file(Path) of
			{error, Reason} ->
		        {error, Reason};
			{ok, Content} ->
				file_helpers:download(File#uce_file.id, Content)
			end
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

delete([Org, Meeting, Id], [EUid], _) ->
    case uce_acl:check(EUid, "file", "delete", [Org, Meeting], [{"id", Id}]) of
	{ok, true} ->
	    case uce_file:delete(Id) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
