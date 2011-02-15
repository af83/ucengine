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
                callbacks=[{?MODULE, add,
                            ["uid", "sid", "_filename", "_uri", "metadata"],
                            [required, required, required, required, []],
                            [string, string, string, string, dictionary]}]},
     
     #uce_route{method='GET',
                regexp="/file/([^/]+)",
                callbacks=[{?MODULE, list,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},
     
     #uce_route{method='GET',
                regexp="/file/([^/]+)/([^/]+)",
                callbacks=[{?MODULE, get,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},
     
     #uce_route{method='DELETE',
                regexp="/file/([^/]+)/([^/]+)",
                callbacks=[{?MODULE, delete,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]}].

add(Domain, [Meeting], [Uid, Sid, Name, Uri, Metadata], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "file", "add", {Meeting, Domain}),
    {ok, Id} = uce_file:add(#uce_file{location={Meeting, Domain},
                                      domain=Domain,
                                      name=Name,
                                      uri=Uri,
                                      metadata=Metadata}),
    {ok, File} = uce_file:get(Id),
    {ok, FileInfo} = file:read_file_info(get_path(File#uce_file.uri)),
    uce_event:add(#uce_event{domain=Domain,
                             location={Meeting, Domain},
                             from={Uid, Domain},
                             type="internal.file.add",
                             metadata=[ {"id", File#uce_file.id},
                                        {"name", File#uce_file.name},
                                        {"size", integer_to_list(FileInfo#file_info.size)},
                                        {"mime", File#uce_file.mime}]}),
    file_helpers:upload(File#uce_file.id).

list(Domain, [Meeting], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "file", "list", {Meeting, Domain}),
    {ok, Files} = uce_file:list({Meeting, Domain}),
    json_helpers:json({array, [file_helpers:to_json(File) || File <- Files]}).

%%
%% @doc Get real path from encoded uri of record uce_file
%% @spec (Uri::list) -> list
%%
get_path(Uri) ->
    re:replace(Uri, "file\:\/", config:get(datas), [{return, list}]).

get(Domain, [Meeting, Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "file", "get", {Meeting, Domain}, [{"id", Id}]),
    {ok, File} = uce_file:get(Id),
    Path = get_path(File#uce_file.uri),
    case file:read_file(Path) of
        {error, Reason} ->
            throw({error, Reason});
        {ok, Content} ->
            file_helpers:download(File#uce_file.id, Content)
    end.

delete(Domain, [Meeting, Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "file", "delete", {Meeting, Domain}, [{"id", Id}]),
    {ok, deleted} = uce_file:delete(Id),
    json_helpers:ok().
