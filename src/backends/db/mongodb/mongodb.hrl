-define(DEFAULT_MONGODB_PORT, 27017).
-define(DEFAULT_MONGODB_NAME, "uce").

-define(MONGO_POOL, fun() ->
			    {PoolId, _} = config:get(mongodb),
			    PoolId
		    end()).


% TODO: get rid of this record
-record(collection, {
	  name,
	  fields, %% [{key1,value1}, {key2, value2}, ...]
	  index=[]
	 }).
