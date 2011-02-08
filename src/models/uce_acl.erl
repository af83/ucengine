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
         check/4,
         check/5,
         check/6,
         trigger/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(Domain, #uce_acl{uid=Uid, location=Location} = ACL) ->
    case location_helpers:exists(Domain, Location) of
	false ->
	    {error, not_found};
	true ->
	    case uce_user:get(Domain, Uid) of
		{error, Error} ->
		    {error, Error};
		_ ->
		    ?DB_MODULE:add(Domain, ACL)
	    end
    end.

delete(Domain, Uid, Object, Action, Location, Conditions) ->
    ?DB_MODULE:delete(Domain, Uid, Object, Action, Location, Conditions).

check(Domain, Uid, Object, Action) ->
    check(Domain, Uid, Object, Action, [""]).
check(Domain, Uid, Object, Action, Location) ->
    check(Domain, Uid, Object, Action, Location, []).
check(Domain, Uid, Object, Action, Location, Conditions) ->
    case ?DB_MODULE:list(Domain, Uid, Object, Action) of
	{error, Reason} ->
	    {error, Reason};
	{ok, ACL} ->
	    case filter_location(ACL, Location) of
		[] ->
		    {ok, false};
		FilteredACL ->
		    check_conditions(FilteredACL, Conditions)
	    end
    end.

filter_location(ACL, [RequiredMeeting]) ->
    lists:filter(fun(#uce_acl{location=[Meeting]}) ->
			 if
			     Meeting == RequiredMeeting ->
				 true;
			     Meeting == "" ->
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

trigger(Domain, #uce_event{type=Type,
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
                                  uce_acl:add(Domain, #uce_acl{object=Object,
                                                               action=Action,
                                                               conditions=NewConditions,
                                                               uid=From})
                          end,
                          [Rule || {_, Rule} <- Rules]),
            ok;
        _ ->
            ok
    end.
