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
-module(acl_controller).

-export([init/0, check/4, add/4, delete/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='GET',
                regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
                callback={?MODULE, check,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"conditions", [], dictionary}]}},

     #uce_route{method='PUT',
                regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
                callback={?MODULE, add,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"conditions", [], dictionary}]}},

     #uce_route{method='DELETE',
                regexp="/user/([^/]+)/acl/([^/]+)/([^/]+)/?([^/]+)?/?",
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"conditions", [], dictionary}]}}].


check(Domain, [To, Object, Action], Params, Arg) ->
    check(Domain, [To, Object, Action, ""], Params, Arg);
check(Domain, [To, Object, Action, Meeting], [Uid, Sid, Conditions], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, {Uid, Domain}, "acl", "check"),
    case uce_acl:check(Domain, {To, Domain}, Object, Action, {Meeting, Domain}, Conditions) of
        {ok, true} ->
            json_helpers:true(Domain);
        {ok, false} ->
            json_helpers:false(Domain)
    end.

add(Domain, [To, Object, Action], Params, Arg) ->
    add(Domain, [To, Object, Action, ""], Params, Arg);
add(Domain, [To, Object, Action, Meeting], [Uid, Sid, Conditions], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain, 
                                {Uid, Domain}, "acl", "add", {Meeting, Domain},
                                [{"action", Action},
                                 {"object", Object},
                                 {"meeting", Meeting}]),
    {ok, created} = uce_acl:add(Domain,
                                #uce_acl{user={To, Domain},
                                         action=Action,
                                         object=Object,
                                         location={Meeting, Domain},
                                         conditions=Conditions}),

    {ok, _Id} =  uce_event:add(Domain,
                               #uce_event{id={none, Domain},
                                          from={To, Domain},
                                          type="internal.acl.add",
                                          location={Meeting, Domain},
                                          metadata=[{"action", Action},
                                                    {"object", Object}] ++ Conditions}),
    json_helpers:created(Domain).

delete(Domain, [To, Object, Action], Params, Arg) ->
    delete(Domain, [To, Object, Action, "", ""], Params, Arg);
delete(Domain, [To, Object, Action, Meeting], [Uid, Sid, Conditions], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_acl:assert(Domain,
                                {Uid, Domain}, "acl", "delete", {Meeting, Domain},
                                [{"action", Action},
                                 {"object", Object},
                                 {"meeting", Meeting}]),
    {ok, deleted} = uce_acl:delete(Domain, {To, Domain}, Object, Action, {Meeting, Domain}, Conditions),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id={none, Domain},
                                       from={To, Domain},
                                       type="internal.acl.delete",
                                       location={Meeting, Domain},
                                       metadata=[{"action", Action},
                                                 {"object", Object}] ++ Conditions}),
    json_helpers:ok(Domain).
