-module(uce_org_mnesia).

-author('victor.goya@af83.com').

-export([init/0,
	 add/1,
	 update/1,
	 get/1,
	 delete/1,
	 list/0]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_org,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_org)}]).

add(#uce_org{}=Org) ->
    case mnesia:transaction(fun() ->
			       mnesia:write(Org)
		       end) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

update(#uce_org{}=Org) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Org)
			    end) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(Name) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_org, Name)
			    end) of
	{atomic, [Record]} ->
	    Record;
	{atomic, _} ->
	    {error, not_found};
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(Name) ->
    case mnesia:transaction(fun() ->
				    mnesia:delete({uce_org, Name})
			    end) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {aborted, Reason}
    end.

list() ->
    ets:tab2list(uce_org).
