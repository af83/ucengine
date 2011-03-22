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
-module(uce_access).

-author('victor.goya@af83.com').

-export([exists/2,
         delete/2,
         assert/5,
         assert/6,
         check/5,
         check/6]).

-include("uce.hrl").

exists(#uce_access{} = Access, ACL) ->
    case ?MODULE:delete(Access, ACL) of
        ACL ->
            false;
        _ ->
            true
    end.

delete(#uce_access{} = Access, ACL) ->
    lists:filter(fun(#uce_access{} = RoleAccess) ->
                         if
                             RoleAccess#uce_access.object /= Access#uce_access.object ->
                                 true;
                             RoleAccess#uce_access.action /= Access#uce_access.action ->
                                 true;
                             length(Access#uce_access.conditions) /=
                             length(RoleAccess#uce_access.conditions) ->
                                 true;
                             true ->
                                 case lists:subtract(Access#uce_access.conditions,
                                                     RoleAccess#uce_access.conditions) of
                                     [] ->
                                         false;
                                     _ ->
                                         true
                                 end
                         end
                 end,
                 ACL).

assert(Domain, User, Location, Object, Action) ->
    assert(Domain, User, Location, Object, Action, []).
assert(Domain, User, Location, Object, Action, Conditions) ->
    case check(Domain, User, Location, Object, Action, Conditions) of
        {ok, false} ->
            throw({error, unauthorized});
        {ok, true} ->
            {ok, true}
    end.

check(Domain, User, Location, Object, Action) ->
    check(Domain, User, Location, Object, Action, []).
check(Domain, User, Location, Object, Action, Conditions) ->
    case uce_user:acl(Domain, User, Location) of
        {ok, []}  ->
            {ok, false};
        {ok, ACL} ->
            FilteredACL =
                lists:filter(fun(#uce_access{object=AccessObject,
                                             action=AccessAction}) ->
                                     if
                                         AccessObject == "all",
                                         AccessAction == "all" ->
                                             true;
                                         AccessObject == "all", AccessAction == Action ->
                                             true;
                                         AccessObject == Object, AccessAction == "all" ->
                                             true;
                                         AccessObject == Object, AccessAction == Action ->
                                             true;
                                         true ->
                                             false
                                     end
                             end,
                             ACL),
            check_conditions(FilteredACL, Conditions)
    end.

% All the ACL have been checked and none of them satisfied the conditions
check_conditions([], _) ->
    {ok, false};
% There is no conditions, so the access is authorized
check_conditions(_, []) ->
    {ok, true};
check_conditions([#uce_access{conditions=Conditions}|Tail], Required) ->
    case Conditions of
        [] ->
            {ok, true};
        _ ->
            case lists:filter(fun({Key, Value}) ->
                                      case lists:keyfind(Key, 1, Required) of
                                          {Key, Value} ->
                                              true;
                                          {Key, '_'} ->
                                              true;
                                          false ->
                                              true;
                                          _ ->
                                              false
                                      end
                              end,
                              Conditions) of
                [] ->
                    check_conditions(Tail, Required);
                _ ->
                    {ok, true}
            end
    end.
