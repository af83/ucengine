-module(uce_infos_mnesia).

-behaviour(gen_uce_infos).
%% gen_uce_infos api
-export([get/0, update/1]).

-export([init/0, drop/0]).

-record(uce_infos, {
          id = none,
          metadata = []}).

init() ->
    catch mnesia:create_table(uce_infos,
			      [{disc_copies, [node()]},
			       {type, bag},
			       {attributes, record_info(fields, uce_infos)}]).

get() ->
    case mnesia:transaction(fun() ->
                                    mnesia:match_object(#uce_infos{metadata='_'})
                            end) of
        {atomic, [#uce_infos{metadata = Metadata}]} ->
            {ok, Metadata};
        {atomic, []} ->
            {ok, []};
        {aborted, Reason} ->
            {error, Reason}
    end.

update(Metadata) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(#uce_infos{metadata = Metadata})
			    end) of
	{atomic, _} ->
	    {ok, updated};
	{aborted, Reason} ->
	    {error, Reason}
    end.

drop() ->
    mnesia:clear_table(uce_infos).
