-module(org_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([to_json/1]).

to_json(#uce_org{name=Org, metadata=Metadata}) ->
	{struct, [{name, Org}, {metadata, {struct, Metadata}}]}.
