-module(uce_meeting_mnesia).

-author('victor.goya@af83.com').

-export([init/0,
	 add/1,
	 delete/1,
	 get/1,
	 update/1,
	 list/1]).

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
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(#uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
				    mnesia:delete({uce_meeting, Meeting#uce_meeting.id})
			    end) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_meeting, Id)
			    end) of
	{atomic, [Record]} ->
	    Record;
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
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

list(Org) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_meeting{id=[Org, '_'],
								       start_date='_',
								       end_date='_',
								       roster='_',
								       metadata='_'})
			    end) of
	{atomic, Meetings} ->
	    Meetings;
	{aborted, Reason} ->
	    {error, Reason}
    end.
