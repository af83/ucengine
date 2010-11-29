-module(uce_org_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 update/1,
	 get/1,
	 delete/1,
	 list/0,
	 from_collection/1,
	 to_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_org{}=Org) ->
    case catch emongo:insert(?MONGO_POOL, "uce_org", ?MODULE:to_collection(Org)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

update(#uce_org{}=Org) ->
    case catch emongo:update(?MONGO_POOL,
			     "uce_org",
			     [{"name", Org#uce_org.name}],
			     ?MODULE:to_collection(Org)) of

	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

get(Name) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_org", [{"name", Name}], [{limit, 1}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[Collection] ->
	    ?MODULE:from_collection(Collection);
	_ ->
	    {error, not_found}
    end.

delete(Name) ->
    case catch emongo:delete(?MONGO_POOL, "uce_org", [{"name", Name}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

list() ->
    case catch emongo:find_all(?MONGO_POOL, "uce_org") of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    lists:map(fun(Collection) ->
			      ?MODULE:from_collection(Collection)
		      end,
		      Collections)
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection), ["name", "metadata"]) of
	[Name, Metadata] ->
	    #uce_org{name=Name,
		       metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.
	
to_collection(#uce_org{name=Org, metadata=Metadata}) ->
    [{"name", Org}, {"metadata", Metadata}].
