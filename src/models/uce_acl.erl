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

-export([add/1,
         delete/5,
         assert/3,
         assert/4,
         assert/5,
         check/3,
         check/4,
         check/5,
         trigger/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_acl{user=User, location=Location} = ACL) ->
    case location_helpers:exists(Location) of
        false ->
            throw({error, not_found});
        true ->
            case uce_user:exists(User) of
                true ->
                    ?DB_MODULE:add(ACL);
                false ->
                    throw({error, not_found})
            end
    end.

delete(User, Object, Action, Location, Conditions) ->
    MeetingExists = uce_meeting:exists(Location),
    UserExists = uce_user:exists(User),

    if
        MeetingExists,
        UserExists ->
            case Location of
                {"", _} ->
                    ?DB_MODULE:delete(User, Object, Action, Location, Conditions),
                    ?DB_MODULE:delete(User, Object, Action, {"", ""}, Conditions);
                _ ->
                    ?DB_MODULE:delete(User, Object, Action, Location, Conditions)
            end;
        true ->
            throw({error, not_found})
    end.

assert(User, Object, Action) ->
    assert(User, Object, Action, {"", ""}).
assert(User, Object, Action, Location) ->
    assert(User, Object, Action, Location, []).
assert(User, Object, Action, Location, Conditions) ->
    case check(User, Object, Action, Location, Conditions) of
        {ok, false} ->
            throw({error, unauthorized});
        {ok, true} ->
            {ok, true}
    end.

check(User, Object, Action) ->
    check(User, Object, Action, {"", ""}).
check(User, Object, Action, Location) ->
    check(User, Object, Action, Location, []).
check(User, Object, Action, Location, Conditions) ->
    case ?DB_MODULE:list(User, Object, Action) of
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

replace_conditions([], _) ->
    [];
replace_conditions([{Key, Value}|Tail], Variables) ->
    Condition = case is_atom(Value) of
                    true ->
                        case lists:keyfind(atom_to_list(Value), 1, Variables) of
                            false ->
                                ?DEBUG("Cannot set ACL: unknown key: ~p~n", [Key]),
                                [];
                            NewCondition ->
                                [NewCondition]
                        end;
                    false ->
                        [{Key, Value}]
                end,
    Condition ++ replace_conditions(Tail, Variables).

trigger(#uce_event{type=Type,
                   location=Location,
                   from=From,
                   metadata=UnsecureMetadata}) ->
    case lists:keyfind(Type, 1, config:get(acl)) of
        {Type, Rules} ->
            lists:foreach(fun({Object, Action, Conditions}) ->
                                  Metadata =
                                      lists:filter(fun({Key, _}) ->
                                                           case Key of
                                                               "location" ->
                                                                   false;
                                                               "from" ->
                                                                   false;
                                                               _ ->
                                                                   true
                                                           end
                                                   end,
                                                   UnsecureMetadata),
                                  NewConditions =
                                      replace_conditions(Conditions, Metadata ++
                                                             [{"location", Location},
                                                              {"from", From}]),
                                  uce_acl:add(#uce_acl{object=Object,
                                                       action=Action,
                                                       conditions=NewConditions,
                                                       user=From})
                          end,
                          Rules),
            ok;
        _ ->
            ok
    end.
