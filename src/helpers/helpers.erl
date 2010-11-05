-module(helpers).

-author('victor.goya@af83.com').

-export([paginate/4]).

paginate(UnsortedList, Count, Page, Order) ->
    List = case Order of
	       asc ->
		   UnsortedList;
	       desc ->
		   lists:reverse(UnsortedList)
	   end,
    if
	Count == infinity ->
	    List;
	true ->
	    Start = (Count * (Page - 1)) + 1,
	    if
		Start > length(List) ->
		    [];
		true ->
		    lists:sublist(List, Count * Page, Count)
	    end
    end.

