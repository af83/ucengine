-define(DB_MODULE, (fun() ->
			list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(config:get(db)))
		end())).

-define(SEARCH_MODULE, (fun() ->
			     list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(config:get(search)) ++ "_search")
		     end())).
