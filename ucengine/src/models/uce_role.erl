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
-module(uce_role).

-export([add/2,
         delete/2,
         update/2,
         get/2,
         exists/2,
         acl/2,
         add_access/3,
         delete_access/3,
         drop/1]).

-include("uce.hrl").

%
% Public api
%

-spec add(domain(), #uce_role{}) -> {ok, created} | erlang:throw({error, conflict}).
add(Domain, #uce_role{id=Id} = Role) ->
    case exists(Domain, Id) of
        true ->
            throw({error, conflict});
        false ->
            internal_add(Domain, Role)
    end.

-spec update(domain(), #uce_role{}) -> {ok, updated} | erlang:throw({error, not_found}).
update(Domain, #uce_role{id=Id} = Role) ->
    case exists(Domain, Id) of
        true ->
            internal_update(Domain, Role);
        false ->
            throw({error, not_found})
    end.

-spec delete(domain(), string()) -> {ok, deleted} | erlang:throw({error, not_found}).
delete(Domain, Id) ->
    case exists(Domain, Id) of
        true ->
            internal_delete(Domain, Id);
        false ->
            throw({error, not_found})
    end.

-spec get(domain(), string()) -> {ok, #uce_role{}} | erlang:throw({error, not_found}).
get(Domain, Id) ->
    internal_get(Domain, Id).

-spec exists(domain(), string()) -> true | false | erlang:throw({error, atom()}).
exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        _ ->
            true
    end.

-spec acl(domain(), string()) -> {ok, updated} | erlang:throw({error, not_found}).
acl(Domain, Id) ->
    {ok, Role} = get(Domain, Id),
    {ok, Role#uce_role.acl}.

-spec add_access(domain(), string(), #uce_access{}) -> {ok, updated} | erlang:throw({error, not_found}).
add_access(Domain, Id, #uce_access{} = Access) ->
    {ok, Role} = get(Domain, Id),
    case uce_access:exists(Access, Role#uce_role.acl) of
        true ->
            {ok, updated};
        false ->
            update(Domain, Role#uce_role{acl=(Role#uce_role.acl ++ [Access])})
    end.

-spec delete_access(domain(), string(), #uce_access{}) -> {ok, updated} | erlang:throw({error, not_found}).
delete_access(Domain, Id, #uce_access{} = Access) ->
    {ok, Role} = get(Domain, Id),
    ACL = case uce_access:exists(Access, Role#uce_role.acl) of
              true ->
                  uce_access:delete(Access, Role#uce_role.acl);
              false ->
                  Role#uce_role.acl
          end,
    update(Domain, Role#uce_role{acl=ACL}).

-spec drop(domain()) -> true.
drop(_Domain) ->
    delete_table().

%
% Private functions
%

-define(TAB, uce_role_cache).

init_table() ->
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [set, public, {keypos, 1}, named_table]);
        _ ->
            ok
    end.

delete_table() ->
    case ets:info(?TAB) of
        undefined ->
            ok;
        _ ->
            ets:delete(?TAB)
    end.

cache_add(Domain, #uce_role{id=Id} = Role) ->
    ets:insert(?TAB, {{Domain, Id}, Role}).

cache_update(Domain, Role) ->
    ?INFO_MSG("cache update", []),
    cache_add(Domain, Role).

cache_delete(Domain, Id) ->
    ets:delete(?TAB, {Domain, Id}).

cache_get(Domain, Id) ->
    case ets:lookup(?TAB, {Domain, Id}) of
        [] ->
            undefined;
        [{{Domain, Id}, Role}] ->
            Role
    end.

internal_add(Domain, Role) ->
    init_table(),
    cache_add(Domain, Role),
    (db:get(?MODULE, Domain)):add(Domain, Role).

internal_update(Domain, Role) ->
    init_table(),
    cache_update(Domain, Role),
    (db:get(?MODULE, Domain)):update(Domain, Role).

internal_delete(Domain, Id) ->
    init_table(),
    cache_delete(Domain, Id),
    (db:get(?MODULE, Domain)):delete(Domain, Id).

internal_get(Domain, Id) ->
    init_table(),
    case cache_get(Domain, Id) of
        undefined ->
            case (db:get(?MODULE, Domain)):get(Domain, Id) of
                {ok, Role} ->
                    cache_add(Domain, Role),
                    {ok, Role};
                Error ->
                    Error
            end;
        Role ->
            {ok, Role}
    end.
