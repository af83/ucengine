-module(uce_file_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 list/1,
	 get/1,
	 delete/1,
	 from_collection/1,
	 to_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_file{} = File) ->
    case catch emongo:insert(?MONGO_POOL, "uce_file", ?MODULE:to_collection(File)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

list(Location) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_file", [{"location", Location}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Files ->
	    [?MODULE:from_collection(File) || File <- Files]
    end.

get(Id) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_file", [{"id", Id}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[File] ->
	    ?MODULE:from_collection(File);
	_ ->
	    {error, not_found}
    end.

delete(Id) ->
    case catch emongo:delete(?MONGO_POOL, "uce_file", [{"id", Id}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["id", "location", "name", "uri", "metadata"]) of
	[Id, Location, Name, Uri, Metadata] ->	
	    #uce_file{id=Id,
			name=Name,
			location=Location,
			uri=Uri,
			metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_file{id=Id, name=Name, location=Location, uri=Uri, metadata=Metadata}) ->
    [{"id", Id},
     {"location", Location},
     {"name", Name},
     {"uri", Uri},
     {"metadata", Metadata}].
