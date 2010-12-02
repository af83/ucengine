-define(PUBSUB_MODULE, (fun() ->
				list_to_atom(atom_to_list(config:get(pubsub)) ++ "_pubsub")
			end())).
