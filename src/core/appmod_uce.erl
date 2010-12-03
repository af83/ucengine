-module(appmod_uce).

-author('victor.goya@af83.com').

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1, call_handlers/4, convert_param/2]).

convert_param(Param, Type)
  when is_atom(Type) ->
    convert_param(Param, [Type]);
convert_param(_, []) ->
    {error, bad_parameters};
convert_param(Param, [Type|Tail]) ->
    Result = if
		 Type == string ->
		     Param;
		 Type == integer,
		 is_integer(Param) ->
		     Param;
		 Type == integer,
		 is_integer(Param) == false ->
		     case string:to_integer(Param) of
			 {error, _} ->
			     {error, bad_parameters};
			 {Integer, _} ->
			     Integer
		     end;
		 Type == atom,
		 is_atom(Param)->
		     Param;
		 Type == atom,
		 is_atom(Param) == false ->
		     case catch list_to_atom(Param) of
			 Atom when is_atom(Atom) ->
			     Atom;
			 _ ->
			     {error, bad_parameters}
		     end;
		 Type == dictionary ->
		     Param
	     end,
    case Result of
	{error, _} ->
	    convert_param(Param, Tail);
	_ ->
	    Result
    end.

convert([], []) ->
    [];
convert([RawParam|ParamTail], [Types|TypeTail]) ->
    case convert_param(RawParam, Types) of
	{error, Reason} ->
	    {error, Reason};
	Param ->
	    case convert(ParamTail, TypeTail) of
		{error, Reason} ->
		    {error, Reason};
		Remaining ->
		    [Param] ++ Remaining
	    end
    end.

exists([], [], []) ->
    true;
exists([Param|ParamTail], [Object|ObjectTail], [Default|DefaultTail]) ->
    Exists = case Param of
		 Default ->
		     true;
		 _ ->
		     case Object of
			 any ->
			     true;
			 user ->
			     uce_user:exists(Param);
			 presence ->
			     uce_presence:exists(Param);
			 event ->
			     uce_event:exists(Param)
		     end
	     end,
    case Exists of
	false ->
	    false;
	true ->
	    exists(ParamTail, ObjectTail, DefaultTail)
    end.


validate(Query, ParamsList, ParamsDefault, Types, Objects) ->
    case utils:get(Query, ParamsList, ParamsDefault) of
	{error, Reason} ->
	    {error, Reason};
	RawParams ->
	    case lists:member(required, RawParams) of
		true ->
		    {error, missing_parameters};
		false ->
		    case convert(RawParams, Types) of
		     	{error, Reason} ->
		     	    {error, Reason};
		     	Params ->
			    case exists(Params, Objects, ParamsDefault) of
				true ->
				    {ok, Params};
				false ->
				    {error, not_found}
			    end
		    end
	    end
    end.

call_handlers([], _, _, _) ->
    {ok, noreply};
call_handlers([{Module, Function, ParamsList, ParamsDefault, Types, Objects}|Tl], Query, Match, Arg) ->
    case validate(Query, ParamsList, ParamsDefault, Types, Objects) of
	{error, Reason} ->
	    json_helpers:error(Reason);
	{ok, Params} ->
	    ?DEBUG("Call ~p:~p matching ~p with ~p~n", [Module, Function, Match, Params]),
	    case catch Module:Function(Match, Params, Arg) of
		{ok, continue} ->
		    ?MODULE:call_handlers(Tl, Query, Match, Arg);
		{error, Reason} ->
		    ?ERROR_MSG("Error: ~p:~p: ~p~n", [Module, Function, Reason]),
		    json_helpers:error(Reason);
		{'EXIT', {function_clause, [{Module, Function,_}|_]}} ->
		    ?ERROR_MSG("Error: ~p:~p: not_found~n", [Module, Function]),
		    json_helpers:error(not_found);
		{'EXIT', Reason} ->
		    ?ERROR_MSG("Error: ~p:~p: ~p~n", [Module, Function, Reason]),
		    json_helpers:error(Reason);
		{ehtml, _} = HTML ->
		    HTML;
		{streamcontent_from_pid, _, _} = Stream ->
		    Stream;
		Response when is_list(Response) ->
		    Response;
		Error ->
		    ?ERROR_MSG("Error: ~p:~p: ~p~n", [Module, Function, Error]),
		    json_helpers:unexpected_error()
	    end
    end.

out(#arg{} = Arg) ->
    case uce_http:parse(Arg) of
	{error, Reason} ->
	    json_helpers:error(Reason);
	{get_more, _, _} = State ->
	    State;
	{Method, Path, Query} ->
	    process(Method, Path, Query, Arg);
	_ ->
	    json_helpers:unexpected_error()
    end.

process(Method, Path, Query, Arg) ->
    case routes:get(Method, Path) of
	{ok, Match, Handlers} ->	    
	    ?MODULE:call_handlers(Handlers, Query, Match, Arg);
	{error, Reason} ->
	    json_helpers:error(Reason)
    end.
