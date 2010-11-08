-module(doc_controller).

-export([init/0, view/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Documentation",
		title="View documentation",
		path="/doc.format",
		method='GET',
		regexp="/doc\.(.+)",
		callbacks=[{?MODULE, view, [], [], []}]}].

format([{Module, Route}|Tail], "html") ->
    #uce_route{title=Title,
	       desc=Description,
	       path=Path,
	       method=Method,
	       regexp=Regexp,
	       callbacks=Callbacks} = Route,
    [{p, [],
     [{h2, [], Title},
      {p, [], Description},
      {ul, [],
       {li, [],
     	{p, [], "Method:" ++ atom_to_list(Method)}
       }
      }]
     }] ++ format(Tail, "html").

view([Format], _, _) ->
    Title = "API Documentation",
    Routes = routes:list(),
    Body = format(Routes, Format),
    {ehtml, [{h1, [], Title}, Body]}.
