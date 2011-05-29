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

-export([init/0, add/4, list/4, get/4, delete/4]).

-include("uce.hrl").

-include_lib("kernel/include/file.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/file/([^/]+)",
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"content", required, file},
                           {"metadata", [], dictionary},
                           {"forceContentType", "application/json", string}]}},

     #uce_route{method='GET',
                regexp="/file/([^/]+)",
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"order", asc, atom}]}},

     #uce_route{method='GET',
                regexp="/file/([^/]+)/([^/]+)",
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='DELETE',
                regexp="/file/([^/]+)/([^/]+)",
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string}]}}].


add(Domain, [Meeting], [Uid, Sid, FileUploaded, Metadata, ForceContentType], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Meeting, Domain}, "file", "add"),
    {ok, Id} = uce_file:add(Domain, #uce_file{id={none, Domain},
                                              location={Meeting, Domain},
                                              name=FileUploaded#file_upload.filename,
                                              uri=FileUploaded#file_upload.uri,
                                              datetime=utils:now(),
                                              metadata=Metadata}),

    {ok, File} = uce_file:get(Domain, Id),
    {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
    {FileId, Domain} = File#uce_file.id,
    EventMetadata = utils:proplist_merge([{"id", FileId},
                                          {"domain", Domain},
                                          {"name", File#uce_file.name},
                                          {"size", integer_to_list(FileInfo#file_info.size)},
                                          {"mime", File#uce_file.mime}], Metadata),
    uce_event:add(Domain,
                  #uce_event{id={none, Domain},
                             location={Meeting, Domain},
                             from={Uid, Domain},
                             type="internal.file.add",
                             metadata=EventMetadata}),
    %% In old webbrowser we cannot send file with xmlhttprequest, so we send a
    %% file via an iframe, and if we reply with a content-type
    %% 'application/json', browser show a popup allowing users to select the
    %% correct programm to show it. This is very annoying.
    case ForceContentType of
        "application/json" ->
            json_helpers:created(Domain, FileId);
        ContentType ->
            json_helpers:format_response(201, ContentType, cors_helpers:format_cors_headers(Domain), {struct, [{result, FileId}]})
    end.

list(Domain, [Meeting], [Uid, Sid, Order], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Meeting, Domain}, "file", "list"),
    {ok, Files} = uce_file:list(Domain, {Meeting, Domain}, Order),
    json_helpers:json(Domain, {array, [file_helpers:to_json(File) || File <- Files]}).

%%
%% @doc Get real path from encoded uri of record uce_file
%% @spec (Uri::list) -> list
%%
get_path(Uri) ->
    re:replace(Uri, "file\:\/\/", "", [{return, list}]).

get(Domain, [Meeting, Id], [Uid, Sid], _) ->
    NormalizedId = unicode_helpers:normalize_unicode(Id),
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Meeting, Domain}, "file", "get", [{"id", Id}]),
    {ok, File} = uce_file:get(Domain, {NormalizedId, Domain}),
    Path = get_path(File#uce_file.uri),
    case file:read_file(Path) of
        {error, Reason} ->
            throw({error, Reason});
        {ok, Content} ->
            {FileId, _} = File#uce_file.id,
            file_helpers:download(Domain, FileId, Content)
    end.

delete(Domain, [Meeting, Id], [Uid, Sid], _) ->
    NormalizedId = unicode_helpers:normalize_unicode(Id),
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {Meeting, Domain}, "file", "delete", [{"id", Id}]),
    {ok, File} = uce_file:get(Domain, {NormalizedId, Domain}),
    {ok, deleted} = uce_file:delete(Domain, {NormalizedId, Domain}),
    uce_event:add(Domain,
                  #uce_event{id={none, Domain},
                             location={Meeting, Domain},
                             from={Uid, Domain},
                             type="internal.file.delete",
                             metadata=[
                                {"id", Id},
                                {"name", File#uce_file.name}]}),
    json_helpers:ok(Domain).
