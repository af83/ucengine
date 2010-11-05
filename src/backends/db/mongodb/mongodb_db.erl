-module(mongodb_db).

-author('victor.goya@af83.com').

-export([init/1,
	 terminate/0]).

-include("uce.hrl").
-include("mongodb.hrl").

init({Pool, MongoPoolInfos}) ->
    case utils:get(MongoPoolInfos,
		   [size, host, port, database],
		   [1, "localhost", ?DEFAULT_MONGODB_PORT, ?DEFAULT_MONGODB_NAME]) of
	[Size, Host, Port, Name] ->
	    application:start(emongo),
	    emongo:add_pool(Pool, Host, Port, Name, Size),
	    ok;
	_ ->
	    {error, bad_configuration}
    end.

terminate() ->
    ok.
