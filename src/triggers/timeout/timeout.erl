-module(timeout).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([
	 start/1,
	 update/2,
	 clean/1
	]).

-define(DEFAULT_TIMEOUT, 600).

start(Options) ->
    Default = case utils:get(Options, default) of
		  none ->
		      ?DEFAULT_TIMEOUT;
		  Value ->
		      Value
	      end,
    triggers:add(#uce_trigger{location='_',
				type='_',
				action={{timeout, update}, [{"default", Default}]}}),

    spawn(uce_engine, cron, [config:get(presence_timeout), presence, clean, [Default]]),
    ok.

update(Event, Params) ->
    case utils:get(Params, ["default"]) of
	[Default] ->
	    case presence:list(Event#uce_event.from) of
		{error, Reason} ->
		    ?DEBUG("~p: could not update the presence timeout: ~p~n", [Event#uce_event.from, 
									      Reason]);
		Presences ->
		    lists:map(fun(Presence) ->
				      presence:update(Presence#uce_presence{last_activity=utils:now()})
			      end,
			      Presences)
	    end,
	    ok;
	_ ->
	    {error, bad_parameters}
    end.

clean(Default) ->
    lists:foreach(fun(#uce_presence{}=Presence) ->
			  case Presence#uce_presence.last_activity of
			      never ->
				  nothing;
			      _ ->
				  Diff = utils:now() - Presence#uce_presence.last_activity,
				  case Diff > Default of
				      true ->
					  ?MODULE:delete(Presence);
				      false ->
					  nothing
				  end
			  end
		  end,
		  ets:tab2list(uce_presence)),
    ok.

