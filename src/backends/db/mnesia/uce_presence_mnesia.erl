-module(uce_presence_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_presence).

-export([init/0, drop/0]).

-export([add/1,
	 list/1,
	 get/1,
	 delete/1,
	 update/1]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_presence,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_presence)}]).

add(#uce_presence{}=Presence) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Presence)
			    end) of
	{atomic, _} ->
	    {ok, Presence#uce_presence.sid};
	{aborted, Reason} ->
	    {error, Reason}
    end.

list(EUid) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_presence{sid='_',
								      uid=EUid,
								      auth='_',
								      last_activity='_',
								      resource='_',
								      metadata='_'})
			    end) of
	{atomic, []} ->
	    {ok, []};
	{atomic, Records} ->
	    {ok, Records};
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(ESid) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_presence, ESid)
			    end) of
	{atomic, [Record]} ->
	    {ok, Record};
	{atomic, _} ->
	    {error, not_found};
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(Sid) ->
    case mnesia:transaction(fun() ->
				     mnesia:delete({uce_presence, Sid})
			     end) of
	{atomic, _} ->
	    {ok, deleted};
	{aborted, Reason} ->
	    {error, Reason}
    end.

update(#uce_presence{}=Presence) ->
    case mnesia:transaction(fun() ->
				       mnesia:write(Presence)
			    end) of
	{atomic, _} ->
	    {ok, updated};
	{aborted, Reason} ->
	    {error, Reason}
    end.

drop() ->
    mnesia:clear_table(uce_presence).
