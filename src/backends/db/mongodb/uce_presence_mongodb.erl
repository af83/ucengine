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
-module(uce_presence_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_presence).

-export([add/1,
	 list/1,
	 get/1,
	 delete/1,
	 update/1,
         all/0]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_presence{}=Presence) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_presence", to_collection(Presence)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, Presence#uce_presence.sid}
    end.

list(EUid) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_presence", [{"uid", EUid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    Presences = lists:map(fun(Collection) ->
					  from_collection(Collection)
				  end,
				  Collections),
	    {ok, Presences}
    end.

all() ->
    case catch emongo:find_all(?MONGO_POOL, "uce_presence", []) of
        {'EXIT', _} ->
            {error, bad_parameters};
        Collections ->
            Presences = lists:map(fun(Collection) ->
                                          from_collection(Collection)
                                  end,
                                  Collections),
            {ok, Presences}
    end.

get(ESid) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_presence", [{"id", ESid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[Collection] ->
	    {ok, from_collection(Collection)};
	_ ->
	    {error, not_found}
    end.

delete(Sid) ->
    case catch emongo:delete(?MONGO_POOL, "uce_presence", [{"id", Sid}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, deleted}
    end.

update(#uce_presence{}=Presence) ->
    case catch emongo:update_sync(?MONGO_POOL, "uce_presence",
                                  [{"sid", Presence#uce_presence.sid}],
                                  to_collection(Presence)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    {ok, udpated}
    end.


from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection), ["id", "uid", "metadata"]) of
	[ESid, EUid, Metadata] ->
	    #uce_presence{sid=ESid,
			   uid=EUid,
			   metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_presence{} = Presence) ->
    [{"id", Presence#uce_presence.sid},
     {"uid", Presence#uce_presence.uid},
     {"metadata", Presence#uce_presence.metadata}].
