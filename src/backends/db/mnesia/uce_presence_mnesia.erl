-module(uce_presence_mnesia).

-author('victor.goya@af83.com').

-export([init/0,
	 add/1,
	 list/2,
	 get/1,
	 delete/1,
	 update/1]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_presence,
			[{ram_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_presence)}]).

add(#uce_presence{}=Presence) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Presence)
			    end) of
	{atomic, _} ->
	    Presence#uce_presence.sid;
	{aborted, Reason} ->
	    {error, Reason}
    end.

list(EUid, Org) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_presence{sid='_',
								       uid=EUid,
								       auth='_',
								       org=Org,
								       last_activity='_',
								       resource='_',
								       metadata='_'})
			    end) of
	{atomic, []} ->
	    [];
	{atomic, Records} ->
	    Records;
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(ESid) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_presence, ESid)
			    end) of
	{atomic, [Record]} ->
	    Record;
	{atomic, _} ->
	    {error, not_found};
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(#uce_presence{}=Presence) ->
    case mnesia:transaction(fun() ->
				     mnesia:delete({uce_presence, Presence#uce_presence.sid})
			     end) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

update(#uce_presence{}=Presence) ->
    case mnesia:transaction(fun() ->
				       mnesia:write(Presence)
			    end) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.
