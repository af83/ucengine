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
                           {"_filename", required, string},
                           {"_uri", required, string},
                           {"metadata", [], dictionary}]}},

     #uce_route{method='GET',
                regexp="/file/([^/]+)",
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

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


add(Domain, [Meeting], [Uid, Sid, Name, Uri, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, {Uid, Domain}, "file", "add", {Meeting, Domain}),
    {ok, Id} = uce_file:add(Domain, #uce_file{id={none, Domain},
                                              location={Meeting, Domain},
                                              name=Name,
                                              uri=Uri,
                                              metadata=Metadata}),
    {ok, File} = uce_file:get(Domain, Id),
    {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
    {FileId, Domain} = File#uce_file.id,
    uce_event:add(Domain,
                  #uce_event{id={none, Domain},
                             location={Meeting, Domain},
                             from={Uid, Domain},
                             type="internal.file.add",
                             metadata=[ {"id", FileId},
                                        {"domain", Domain}, 
                                        {"name", File#uce_file.name},
                                        {"size", integer_to_list(FileInfo#file_info.size)},
                                        {"mime", File#uce_file.mime}]}),
    json_helpers:created(Domain, File#uce_file.id).

list(Domain, [Meeting], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, {Uid, Domain}, "file", "list", {Meeting, Domain}),
    {ok, Files} = uce_file:list(Domain, {Meeting, Domain}),
    json_helpers:json(Domain, {array, [file_helpers:to_json(File) || File <- Files]}).

%%
%% @doc Get real path from encoded uri of record uce_file
%% @spec (Uri::list) -> list
%%
get_path(Uri) ->
    re:replace(Uri, "file\:\/\/", "", [{return, list}]).

get(Domain, [Meeting, Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, {Uid, Domain}, "file", "get", {Meeting, Domain}, [{"id", Id}]),
    {ok, File} = uce_file:get(Domain, {Id, Domain}),
    Path = get_path(File#uce_file.uri),
    case file:read_file(Path) of
        {error, Reason} ->
            throw({error, Reason});
        {ok, Content} ->
            {FileId, _} = File#uce_file.id,
            file_helpers:download(Domain, FileId, Content)
    end.

delete(Domain, [Meeting, Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, {Uid, Domain}, "file", "delete", {Meeting, Domain}, [{"id", Id}]),
    {ok, deleted} = uce_file:delete(Domain, {Id, Domain}),
    json_helpers:ok(Domain).
