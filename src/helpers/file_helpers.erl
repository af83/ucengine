-module(file_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([to_json/1, download/2, upload/1]).

to_json(#uce_file{id=Id, name=Name, location=Location, uri=Uri, metadata=Metadata}) ->
    JSONLocation = case Location of
		       [Meeting] ->
			   [{meeting, Meeting}];
		       [] ->
			   []
		   end,
    {struct, [{id, Id},
	      {name, Name},
	      {uri, Uri}] ++ JSONLocation ++
	      [{metadata, {struct, Metadata}}]}.

download(Id, Content) ->
    [{status, 200},
     {header, {"Content-Disposition", "filename=" ++ yaws_api:url_encode(Id)}},
     {content, "application/octet-stream", Content}].

upload(Id) ->
    [{status, 201},
     {content, "text/html", "{\"result\": \"" ++ Id ++ "\"}"}].
