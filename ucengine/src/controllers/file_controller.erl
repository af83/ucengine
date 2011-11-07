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
-module(file_controller).

-export([init/0, add/5, list/5, get/5, delete/5]).

-include("uce.hrl").

-include_lib("kernel/include/file.hrl").

init() ->
    [#uce_route{method='POST',
                path=["file", meeting],
                middlewares = [auth,
                               {params, [{"content", required, file},
                                         {"metadata", [], dictionary},
                                         {"forceContentType", "application/json", string}]}],
                callback={?MODULE, add}},

     #uce_route{method='GET',
                path=["file", meeting],
                middlewares = [auth,
                               {params, [{"order", asc, atom}]}],
                callback={?MODULE, list}},

     #uce_route{method='GET',
                path=["file", meeting, id],
                middlewares = [auth],
                callback={?MODULE, get}},

     #uce_route{method='DELETE',
                path=["file", meeting, id],
                middlewares = [auth],
                callback={?MODULE, delete}}].


add(Domain, [{meeting, Meeting}], [Uid, _Sid, FileUploaded, Metadata, ForceContentType], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "file", "add"),
    {ok, Id} = uce_file:add(Domain, #uce_file{location=Meeting,
                                              name=FileUploaded#file_upload.filename,
                                              uri=FileUploaded#file_upload.uri,
                                              datetime=utils:now(),
                                              metadata={struct, Metadata}}),

    {ok, File} = uce_file:get(Domain, Id),
    {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
    EventMetadata = utils:proplist_merge([{"id", Id},
                                          {"domain", Domain},
                                          {"name", File#uce_file.name},
                                          {"size", integer_to_list(FileInfo#file_info.size)},
                                          {"mime", File#uce_file.mime}], Metadata),
    uce_event:add(Domain,
                  #uce_event{id=none,
                             location=Meeting,
                             from=Uid,
                             type="internal.file.add",
                             metadata={struct, EventMetadata}}),
    %% In old webbrowser we cannot send file with xmlhttprequest, so we send a
    %% file via an iframe, and if we reply with a content-type
    %% 'application/json', browser show a popup allowing users to select the
    %% correct programm to show it. This is very annoying.
    case ForceContentType of
        "application/json" ->
            json_helpers:created(Response, Id);
        ContentType ->
            json_helpers:format_response(Response, 201, ContentType, {struct, [{result, Id}]})
    end.

list(Domain, [{meeting, Meeting}], [Uid, _Sid, Order], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "file", "list"),
    {ok, Files} = uce_file:list(Domain, Meeting, Order),
    json_helpers:json(Response, Domain, Files).

%%
%% @doc Get real path from encoded uri of record uce_file
%% @spec (Uri::list) -> list
%%
get_path(Uri) ->
    re:replace(Uri, "file\:\/\/", "", [{return, list}]).

get(Domain, [{meeting, Meeting}, {id, Id}], [Uid, _Sid], _Request, Response) ->
    NormalizedId = unicode_helpers:normalize_unicode(Id),
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "file", "get", [{"id", Id}]),
    {ok, File} = uce_file:get(Domain, NormalizedId),
    Path = get_path(File#uce_file.uri),
    case file:read_file(Path) of
        {error, Reason} ->
            throw({error, Reason});
        {ok, Content} ->
            http_helpers:download(Response, File#uce_file.name, Content)
    end.

delete(Domain, [{meeting, Meeting}, {id, Id}], [Uid, _Sid], _Request, Response) ->
    NormalizedId = unicode_helpers:normalize_unicode(Id),
    {ok, true} = uce_access:assert(Domain, Uid, Meeting, "file", "delete", [{"id", Id}]),
    {ok, File} = uce_file:get(Domain, NormalizedId),
    {ok, deleted} = uce_file:delete(Domain, NormalizedId),
    uce_event:add(Domain,
                  #uce_event{id=none,
                             location=Meeting,
                             from=Uid,
                             type="internal.file.delete",
                             metadata=[
                                {"id", Id},
                                {"name", File#uce_file.name}]}),
    json_helpers:ok(Response).
