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
-module(role_controller).

-export([init/0, add/4, delete/4, add_access/4, delete_access/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/role",
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"name", required, string}]}},

     #uce_route{method='DELETE',
                regexp="/role/([^/]+)",
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='POST',
                regexp="/role/([^/]+)/acl",
                callback={?MODULE, add_access,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"object", "all", string},
                           {"action", "all", string},
                           {"conditions", [], dictionary}]}},

     #uce_route{method='DELETE',
                regexp="/role/([^/]+)/acl/([^/]+)/([^/]+)",
                callback={?MODULE, delete_access,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"conditions", [], dictionary}]}}].

add(Domain, [], [Uid, Sid, Name], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "role", "add", [{"name", Name}]),
    {ok, created} = uce_role:add(Domain, #uce_role{id=Name}),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.role.add",
                                               metadata=[{"name", Name}]}),

    json_helpers:created(Domain).

delete(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "role", "delete", [{"name", Name}]),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.role.delete",
                                               metadata=[{"name", Name}]}),
    {ok, deleted} = uce_role:delete(Domain, Name),
    json_helpers:ok(Domain).

add_access(Domain, [Role], [Uid, Sid, Object, Action, Conditions], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "",
                                   "access", "add", [{"role", Role},
                                                     {"object", Object},
                                                     {"action", Action}]),

    {ok, updated} = uce_role:add_access(Domain, Role,
                                       #uce_access{object=Object,
                                                   action=Action,
                                                   conditions=Conditions}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.access.add",
                                               metadata=[{"role", Role},
                                                         {"action", Action},
                                                         {"object", Object}] ++
                                                   Conditions}),

    json_helpers:ok(Domain).

delete_access(Domain, [Role, Object, Action], [Uid, Sid, Conditions], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "",
                                   "access", "add", [{"role", Role},
                                                     {"object", Object},
                                                     {"action", Action}]),

    {ok, updated} = uce_role:delete_access(Domain, Role,
                                          #uce_access{object=Object,
                                                      action=Action,
                                                      conditions=Conditions}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.access.delete",
                                               metadata=[{"role", Role},
                                                         {"action", Action},
                                                         {"object", Object}] ++
                                                   Conditions}),

    json_helpers:ok(Domain).
