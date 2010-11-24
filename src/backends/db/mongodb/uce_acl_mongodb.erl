-module(uce_acl_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 delete/5,
	 list/3,
	 from_collection/1,
	 to_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_acl{}=ACL) ->
    case catch emongo:insert(?MONGO_POOL, "uce_acl", ?MODULE:to_collection(ACL)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

delete(EUid, Object, Action, Location, Conditions) ->
    case catch emongo:delete(?MONGO_POOL, "uce_acl", [{"uid", EUid},
						      {"object", Object},
						      {"action", Action},
						      {"location", Location},
						      {"conditions", Conditions}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

list(EUid, Object, Action) ->
    case catch emongo:find(?MONGO_POOL, "uce_acl", [{"uid", EUid},
						    {"object", Object},
						    {"action", Action}]) of
	
	{'EXIT', _} ->
	    {error, bad_parameters};
	ACLCollections ->
	    ACL = lists:map(fun(Collection) ->
				    ?MODULE:from_collection(Collection)
			    end,
			    ACLCollections),
	    AllActions = case Action of
			     "all" ->
				 [];
			     _ ->
				 ?MODULE:list(EUid, Object, "all")
			 end,
	    AllObjects = case Object of
			     "all" ->
				 [];
			     _ ->
				 ?MODULE:list(EUid, "all", Action)
			 end,
	    ACL ++ AllActions ++ AllObjects
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["uid", "object", "action", "location", "conditions"]) of
	[EUid, Object, Action, Location, Conditions] ->
	    #uce_acl{uid=EUid,
		     action=Action,
		     object=Object,
		     location=Location,
		     conditions=Conditions};
	_ ->
	    {error, bad_parameters}
    end.
						      
to_collection(#uce_acl{uid=EUid,
		       object=Object,
		       action=Action,
		       location=Location,
		       conditions=Conditions}) ->
    [{"uid", EUid},
     {"object", Object},
     {"action", Action},
     {"location", Location},
     {"conditions", Conditions}].
