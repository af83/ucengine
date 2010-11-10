-module(uce_acl).

-author('victor.goya@af83.com').

-export([
	 add/1,
	 delete/5,
	 check/5,
	 trigger/2
	]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_acl{uid=EUid} = ACL) ->
    case uce_user:get(EUid) of
	{error, Error} ->
	    {error, Error};
	_ ->
	    ?DBMOD:add(ACL)
    end.

delete(EUid, Object, Action, Location, Conditions) ->
    case uce_user:get(EUid) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:delete(EUid, Object, Action, Location, Conditions)
    end.

check(EUid, Object, Action, Location, Conditions) ->
    case ?DBMOD:list(EUid, Object, Action) of
	{error, Reason} ->
	    {error, Reason};
	ACL ->
	    check_conditions(ACL, Conditions)
    end.

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
