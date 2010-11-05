-module(doc_controller).

-export([init/0, view/3]).

-include("uce.hrl").

init() ->
    {doc, [#uce_route{name="View documentation",
			path="/doc",
			method='GET',
			regexp="/doc",
			callbacks=[{?MODULE, view, [], [], []}]}]}.

view(_, _, _) ->
    Title = "API Documentation",
    Body = lists:map(fun({Module, #uce_route{name=Name,
					       description=Description,
					       path=Path,
					       method=Method,
					       regexp=Regexp,
					       callbacks=Callbacks}}) ->
			     {p, [],
			      [{h2, [], Name},
			       {p, [], Description},
			       {ul, [],
				{li, [],
				 {p, [], "Method:" ++ atom_to_list(Method)}
				}
			       }]
			     }
		     end,
		     routes:list()),
    {ehtml, [{h1, [], Title}, Body]}.
