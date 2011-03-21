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
-module(user_controller).

-export([init/0, add/4, update/4, get/4, list/4, delete/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/user",
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"auth", required, string},
                           {"credential", required, string},
                           {"metadata", [], dictionary}]}},
     #uce_route{method='GET',
                regexp="/user",
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='GET',
                regexp="/user/([^/]+)",
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},
     #uce_route{method='PUT',
                regexp="/user/([^/]+)",
                callback={?MODULE, update,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"auth", required, string},
                           {"credential", required, string},
                           {"metadata", [], dictionary}]}},
     #uce_route{method='DELETE',
                regexp="/user/([^/]+)",
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string}]}}].


add(Domain, [], [Name, Auth, Credential, Metadata], _) ->
    {ok, created} = uce_user:add(Domain, #uce_user{id={Name, Domain},
                                                   auth=Auth,
                                                   credential=Credential,
                                                   metadata=Metadata}),

    {ok, _} = uce_event:add(Domain, #uce_event{id={none, Domain},
                                               from={Name, Domain},
                                               location={"", Domain},
                                               type="internal.user.add"}),

    json_helpers:created(Domain).

list(Domain, [], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "user", "list"),
    {ok, Users} = uce_user:list(Domain),
    json_helpers:json(Domain, {array, [user_helpers:to_json(User) || User <- Users]}).

get(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "user", "get", [{"user", Name}]),
    {ok, Record} = uce_user:get(Domain, {Name, Domain}),
    json_helpers:json(Domain, user_helpers:to_json(Record)).

update(Domain, [Name], [Uid, Sid, Auth, Credential, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "user", "update", [{"user", Name},
                                                                                       {"auth", Auth}]),
    {ok, updated} = uce_user:update(Domain, #uce_user{id={Name, Domain},
                                                      auth=Auth,
                                                      credential=Credential,
                                                      metadata=Metadata}),

    {ok, _} = uce_event:add(Domain,
                            #uce_event{id={none, Domain},
                                       from={Name, Domain},
                                       location={"", Domain},
                                       type="internal.user.update"}),

    json_helpers:ok(Domain).

delete(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "user", "delete", [{"user", Name}]),
    {ok, deleted} = uce_user:delete(Domain, {Name, Domain}),
    json_helpers:ok(Domain).
