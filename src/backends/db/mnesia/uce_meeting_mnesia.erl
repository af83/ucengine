
-module(uce_meeting_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_meeting).

-export([init/0, drop/0]).

-export([add/1,
	 delete/1,
	 get/1,
	 update/1,
	 list/0]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_meeting,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_meeting)}]).

add(#uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
			       mnesia:write(Meeting)
		       end) of
	{atomic, _} ->
	    {ok, created};
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:delete({uce_meeting, Id})
			    end) of
	{atomic, ok} ->
	    {ok, deleted};
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_meeting, Id)
			    end) of
	{atomic, [Record]} ->
	    {ok, Record};
	{atomic, _} ->
	    {error, not_found};
	{aborted, Reason} ->
	    {error, Reason}
    end.

update(#uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Meeting)
			    end) of
	{atomic, _} ->
	    {ok, updated};
	{aborted, Reason} ->
	    {error, Reason}
    end.

list() ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_meeting{id=['_'],
								     start_date='_',
								     end_date='_',
								     roster='_',
								     metadata='_'})
			    end) of
	{atomic, Meetings} ->
	    {ok, Meetings};
	{aborted, Reason} ->
	    {error, Reason}
    end.

drop() ->
    mnesia:clear_table(uce_meeting).
