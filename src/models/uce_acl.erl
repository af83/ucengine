-module(uce_acl).

-author('victor.goya@af83.com').

-export([add/1,
	 delete/5,
	 check/5,
	 trigger/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_acl{uid=Uid, location=Location} = ACL) ->
    case location_helpers:exists(Location) of
	false ->
	    {error, not_found};
	true ->
	    case uce_user:get(Uid) of
		{error, Error} ->
		    {error, Error};
		_ ->
		    ?DB_MODULE:add(ACL)
	    end
    end.

delete(Uid, Object, Action, Location, Conditions) ->
    ?DB_MODULE:delete(Uid, Object, Action, Location, Conditions).

check(Uid, Object, Action, Location, Conditions) ->
    case ?DB_MODULE:list(Uid, Object, Action) of
	{error, Reason} ->
	    {error, Reason};
	ACL ->
	    case filter_location(ACL, Location) of
		[] ->
		    false;
		FilteredACL ->
		    check_conditions(FilteredACL, Conditions)
	    end
    end.

filter_location(ACL, [RequiredOrg, RequiredMeeting]) ->
    lists:filter(fun(#uce_acl{location=[Org, Meeting]}) ->
			 if
			     Org == RequiredOrg,
			     Meeting == RequiredMeeting ->
				 true;
			     Org == RequiredOrg,
			     Meeting == "" ->
				 true;
			     Org == "",
			     Meeting == "" ->
				 true;
			     true ->
				 false
			 end
		 end,
		 ACL).

check_conditions([], _) ->
    false;
check_conditions(_, []) ->
    true;
check_conditions([#uce_acl{conditions=Conditions}|Tail], Required) ->
    case Conditions of
	[] ->
	    true;
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
		    true
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

trigger(#uce_event{location=Location,
		   from=From,
		   metadata=UnsecureMetadata},
	#uce_acl{conditions=Conditions} = ACL) ->
    Metadata = lists:filter(fun({Key, _}) ->
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
    NewConditions = replace_conditions(Conditions, Metadata ++
					   [{"location", Location}, {"from", From}]),
    uce_acl:add(ACL#uce_acl{conditions=NewConditions, uid=From}).
