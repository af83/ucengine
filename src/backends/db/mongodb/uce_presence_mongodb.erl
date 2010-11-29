-module(uce_presence_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 list/2,
	 get/1,
	 delete/1,
	 update/1,
	 from_collection/1,
	 to_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_presence{}=Presence) ->
    case catch emongo:insert(?MONGO_POOL, "uce_presence", ?MODULE:to_collection(Presence)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    Presence#uce_presence.sid
    end.

list(EUid, Org) ->
    OrgSelector = case Org of
		      '_' ->
			  [];
		      _ ->
			  [{"org", Org}]
		  end,
    case catch emongo:find_all(?MONGO_POOL, "uce_presence", [{"uid", EUid}] ++ OrgSelector) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    lists:map(fun(Collection) ->
			      ?MODULE:from_collection(Collection)
		      end,
		      Collections)
    end.

get(ESid) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_presence", [{"id", ESid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[Collection] ->
	    ?MODULE:from_collection(Collection);
	_ ->
	    {error, not_found}
    end.

delete(Sid) ->
    case catch emongo:delete(?MONGO_POOL, "uce_presence", [{"id", Sid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

update(#uce_presence{}=Presence) ->
    case catch emongo:update(?MONGO_POOL, "uce_presence",
			     [{"sid", Presence#uce_presence.sid}],
			     ?MODULE:to_collection(Presence)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.


from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection), ["id", "uid", "org", "metadata"]) of
	[ESid, EUid, Org, Metadata] ->
	    #uce_presence{sid=ESid,
			   uid=EUid,
			   org=Org,
			   metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_presence{} = Presence) ->
    [{"id", Presence#uce_presence.sid},
     {"uid", Presence#uce_presence.uid},
     {"org", Presence#uce_presence.org},
     {"metadata", Presence#uce_presence.metadata}].
