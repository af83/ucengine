-module(uce_acl_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_acl).

-export([init/0,
	 add/1,
	 delete/5,
	 list/3]).

-include("uce.hrl").

init() ->
    catch mnesia:create_table(uce_acl,
			      [{disc_copies, [node()]},
			       {type, bag},
			       {attributes, record_info(fields, uce_acl)}]).

add(#uce_acl{}=ACL) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(ACL)
			    end) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(EUid, Object, Action, Location, Conditions) ->
    case exists(EUid, Object, Action, Location, Conditions) of
	false ->
	    {error, not_found};
	true ->
	    case mnesia:transaction(fun() ->
					    mnesia:delete_object(#uce_acl{uid=EUid,
									  object=Object,
									  action=Action,
									  location=Location,
									  conditions=Conditions})
				    end) of
		{atomic, _} ->
		    ok;
		{aborted, Reason} ->
		    {error, Reason}
	    end
    end.
	
list(EUid, Object, Action) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_acl{uid=EUid,
								 object=Object,
								 action=Action,
								 location='_',
								 conditions='_'})
			    end) of
	{aborted, _Reason} ->
	    [];
	{atomic, ACL} ->
	    AllActions = case Action of
			    "all" ->
				[];
			    _ ->
				?MODULE:list(EUid, "all", Object)
			end,
	    AllObjects = case Object of
			     "all" ->
				 [];
			     _ ->
				 ?MODULE:list(EUid, Action, "all")
			 end,
	    ACL ++ AllActions ++ AllObjects
    end.

exists(EUid, Object, Action, Location, Conditions) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_acl{uid=EUid,
								 object=Object,
								 action=Action,
								 location=Location,
								 conditions=Conditions})
			    end) of
	{atomic, [_]} ->
	    true;
	{atomic, _} ->
	    false;
	{aborted, _} ->
	    false
    end.
