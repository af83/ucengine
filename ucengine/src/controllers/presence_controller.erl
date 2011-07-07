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
-module(presence_controller).

-export([init/0, delete/4, get/4, add/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                path=["presence"],
                callback={?MODULE, add,
                          [{"name", required, string},
                           {"credential", "", string},
                           {"timeout", 0, integer},
                           {"metadata", [], dictionary}]}},

     #uce_route{method='GET',
                path=["presence", sid],
                callback={?MODULE, get, []}},

     #uce_route{method='DELETE',
                path=["presence", sid],
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string}]}}].

add(Domain, [], [Name, Credential, Timeout, Metadata], _) ->
    {ok, #uce_user{id = Uid} = User} = uce_user:get_by_name(Domain, Name),
    {ok, true} = uce_access:assert(Domain, Uid, "", "presence", "add"),
    {ok, true} = ?AUTH_MODULE(User#uce_user.auth):assert(User, Credential),
    {ok, Sid} = uce_presence:add(Domain,
                                 #uce_presence{id=none,
                                               user=Uid,
                                               timeout=Timeout,
                                               auth=User#uce_user.auth,
                                               metadata=Metadata}),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       from=User#uce_user.id,
                                       location="",
                                       type="internal.presence.add"}),
    json_helpers:json(Domain, 201, {struct, [{uid, Uid}, {sid, Sid}]}).

get(Domain, [{sid, Sid}], [], _) ->
    case uce_presence:get(Domain, Sid) of
        {ok, Presence} ->
            json_helpers:json(Domain, Presence);
        {error, not_found} ->
            json_helpers:error(Domain, not_found)
    end.

delete(Domain, [{sid, Sid2}], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    case uce_presence:get(Domain, Sid2) of
        {ok, #uce_presence{id=Id, user=User, meetings=Meetings}} ->
            {ok, true} = uce_access:assert(Domain, Uid, "", "presence", "delete",
                                           [{"id", Id}]),
            ok = uce_timeout:clean_meetings(Domain, User, Meetings),
            {ok, deleted} = uce_presence:delete(Domain, Sid2),
            json_helpers:ok(Domain);
        {error, not_found} ->
            json_helpers:error(Domain, not_found)
    end.
