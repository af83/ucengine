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
-module(file_controller).

-export([init/0, add/3, list/3, get/3, delete/3]).

-include("uce.hrl").

-include_lib("kernel/include/file.hrl").

init() ->
    [#uce_route{module="Files",
		method='GET',
		regexp="/file/([^/]+)",
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
		regexp="/file/([^/]+)/([^/]+)",
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
		regexp="/file/([^/]+)",
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
		regexp="/file/([^/]+)/([^/]+)",
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

add(Location, [EUid, Name, Uri, Metadata], Arg) ->
    case uce_acl:check(utils:domain(Arg), EUid, "file", "add", Location) of
	{ok, true} ->
	    case uce_file:add(utils:domain(Arg), #uce_file{location=Location,
                                                       name=Name,
                                                       uri=Uri,
                                                       metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Id} ->
		    % XXX: shall the model returns this precious record of her ?
		    case uce_file:get(utils:domain(Arg), Id) of
			{ok, #uce_file{} = File} ->
                {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
			    uce_event:add(utils:domain(Arg), #uce_event{location=Location,
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

list(Location, [EUid], Arg) ->
    case uce_acl:check(utils:domain(Arg), EUid, "file", "list", Location) of
	{ok, true} ->
	    case uce_file:list(utils:domain(Arg), Location) of
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

get([Meeting, Id], [EUid], Arg) ->
    case uce_acl:check(utils:domain(Arg), EUid, "file", "get", [Meeting], [{"id", Id}]) of
	{ok, true} ->
	    case uce_file:get(utils:domain(Arg), Id) of
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

delete([Meeting, Id], [EUid], Arg) ->
    case uce_acl:check(utils:domain(Arg), EUid, "file", "delete", [Meeting], [{"id", Id}]) of
	{ok, true} ->
	    case uce_file:delete(utils:domain(Arg), Id) of
		{error, Reason} ->
		    {error, Reason};
		{ok, deleted} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
