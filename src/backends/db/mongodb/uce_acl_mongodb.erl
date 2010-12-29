-module(uce_acl_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_acl).

-export([add/1,
	 delete/5,
	 list/3]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_acl{}=ACL) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_acl", to_collection(ACL)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, created}
    end.

delete(Uid, Object, Action, Location, Conditions) ->
    case exists(Uid, Object, Action, Location, Conditions) of
	false ->
	    {error, not_found};
	true ->
	    case catch emongo:delete(?MONGO_POOL, "uce_acl", [{"uid", Uid},
							      {"object", Object},
							      {"action", Action},
							      {"location", Location},
							      {"conditions", Conditions}]) of
		{'EXIT', _} ->
		    {error, bad_parameters};
		_ ->
		    {ok, deleted}
	    end
    end.

list(Uid, Object, Action) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_acl", [{"uid", Uid},
                                                        {"object", Object},
                                                        {"action", Action}]) of
	
	{'EXIT', _} ->
	    {error, bad_parameters};
	ACLCollections ->
	    ACL = lists:map(fun(Collection) ->
				    from_collection(Collection)
			    end,
			    ACLCollections),
	    {ok, AllActions} =
		case Action of
		    "all" ->
			{ok, []};
		    _ ->
			list(Uid, Object, "all")
		end,
	    {ok, AllObjects} =
		case Object of
		    "all" ->
			{ok, []};
		    _ ->
			list(Uid, "all", Action)
		end,
	    {ok, ACL ++ AllActions ++ AllObjects}
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["uid", "object", "action", "location", "conditions"]) of
	[Uid, Object, Action, Location, Conditions] ->
	    #uce_acl{uid=Uid,
		     action=Action,
		     object=Object,
		     location=Location,
		     conditions=Conditions};
	_ ->
	    {error, bad_parameters}
    end.
						      
to_collection(#uce_acl{uid=Uid,
		       object=Object,
		       action=Action,
		       location=Location,
		       conditions=Conditions}) ->
    [{"uid", Uid},
     {"object", Object},
     {"action", Action},
     {"location", Location},
     {"conditions", Conditions}].

exists(Uid, Object, Action, Location, Conditions) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_acl", [{"uid", Uid},
                                                        {"object", Object},
                                                        {"action", Action},
                                                        {"location", Location},
                                                        {"conditions", Conditions}],
			  [{limit, 1}]) of
	{'EXIT', _} ->
	    false;
	[] ->
	    false;
	_ ->
	    true
    end.
