-define(AUTH_MODULE(Module), (fun() ->
				      list_to_atom(Module ++ "_auth")
			      end())).

