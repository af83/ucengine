-module(uce_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 10, 10}, 
	  [{routes,
	    {routes, start_link, []},
	    permanent, brutal_kill, worker, [routes]},
	   {mnesia_pubsub,
	    {mnesia_pubsub, start_link, []},
	    permanent, brutal_kill, worker, [mnesia_pubsub]}]}}.
