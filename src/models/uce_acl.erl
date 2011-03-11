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
-module(uce_acl).

-author('victor.goya@af83.com').

-export([add/2,
         delete/6,
         assert/4,
         assert/5,
         assert/6,
         check/4,
         check/5,
         check/6,
         trigger/2]).

-include("uce.hrl").

add(Domain, #uce_acl{user=User, location=Location} = ACL) ->
    case location_helpers:exists(Domain, Location) of
        false ->
            throw({error, not_found});
        true ->
            case uce_user:exists(Domain, User) of
                true ->
                    apply(db:get(?MODULE, Domain), add, [ACL]);
                false ->
                    throw({error, not_found})
            end
    end.

delete(Domain, User, Object, Action, Location, Conditions) ->
    MeetingExists = uce_meeting:exists(Domain, Location),
    UserExists = uce_user:exists(Domain, User),

    if
        MeetingExists,
        UserExists ->
            case Location of
                {"", _} ->
                    apply(db:get(?MODULE, Domain), delete, [User, Object, Action, Location, Conditions]),
                    apply(db:get(?MODULE, Domain), delete, [User, Object, Action, {"", ""}, Conditions]);
                _ ->
                    apply(db:get(?MODULE, Domain), delete, [User, Object, Action, Location, Conditions])
            end;
        true ->
            throw({error, not_found})
    end.

assert(Domain, User, Object, Action) ->
    assert(Domain, User, Object, Action, {"", ""}).
assert(Domain, User, Object, Action, Location) ->
    assert(Domain, User, Object, Action, Location, []).
assert(Domain, User, Object, Action, Location, Conditions) ->
    case check(Domain, User, Object, Action, Location, Conditions) of
        {ok, false} ->
            throw({error, unauthorized});
        {ok, true} ->
            {ok, true}
    end.

check(Domain, User, Object, Action) ->
    check(Domain, User, Object, Action, {"", ""}).
check(Domain, User, Object, Action, Location) ->
    check(Domain, User, Object, Action, Location, []).
check(Domain, User, Object, Action, Location, Conditions) ->
    case apply(db:get(?MODULE, Domain), list, [User, Object, Action]) of
        {error, Reason} ->
            throw({error, Reason});
        {ok, ACL} ->
            case filter_location(ACL, Location) of
                [] ->
                    {ok, false};
                FilteredACL ->
                    check_conditions(FilteredACL, Conditions)
            end
    end.

filter_location(ACL, RequiredLocation) ->
    lists:filter(fun(#uce_acl{location={Meeting, _Domain} = Location}) ->
                         if
                             Meeting == "" ->
                                 true;
                             Location == RequiredLocation ->
                                 true;
                             true ->
                                 false
                         end
                 end,
                 ACL).

check_conditions([], _) ->
    {ok, false};
check_conditions(_, []) ->
    {ok, true};
check_conditions([#uce_acl{conditions=Conditions}|Tail], Required) ->
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

trigger(Domain, 
        #uce_event{type=Type,
                   location=Location,
                   from=From}) ->
    Acl = case config:get(Domain, acl) of
            undefined ->
                  config:get(acl);
            Val ->
                  Val
          end, 
    case lists:keyfind(Type, 1, Acl) of
        {Type, Rules} ->
            lists:foreach(fun({Object, Action}) ->
                                  uce_acl:add(Domain, 
                                              #uce_acl{object=Object,
                                                       action=Action,
                                                       conditions=[],
                                                       location=Location,
                                                       user=From})
                          end,
                          Rules),
            ok;
        _ ->
            ok
    end.
