-module(tests_utils).

-include("uce.hrl").

-export([ post/2
	  , post/4
	  , get_raw/2
	  , get/1
	  , get/2
	  , put/2
	  , put/4
        , delete/2
        ]).
-export([url_encode/1]).

-define(HTTP_TIMEOUT, 30000).

get_raw(Path, Params) ->
    httpc:request(?BASE_URL ++ Path ++ "?" ++ url_encode(Params), httpc:default_profile()).

get(Path) ->
    {ok, {_, _, JSON}} = httpc:request(?BASE_URL ++ Path, httpc:default_profile()),
    mochijson:decode(JSON).
get(Path, Params) ->
    {ok, {_, _, JSON}} = httpc:request(?BASE_URL ++ Path ++ "?" ++
					   url_encode(Params), httpc:default_profile()),
    mochijson:decode(JSON).

post(Path, Params) ->
    {ok, {_, _, JSON}} = request(Path, post, [], "application/x-www-form-urlencoded", url_encode(Params)),
    mochijson:decode(JSON).

post(Path, Params, ContentType, Body) ->
    {ok, {_, _, JSON}} = request(Path, post, Params, ContentType, Body),
    mochijson:decode(JSON).

put(Path, Params) ->
    {ok, {_, _, JSON}} = request(Path, put, Params, "application/x-www-form-urlencoded", []),
    mochijson:decode(JSON).

put(Path, Params, ContentType, Body) ->
    {ok, {_, _, JSON}} = request(Path, put, Params, ContentType, Body),
    mochijson:decode(JSON).

delete(Path, Params) ->
    {ok, {_, _, JSON}} = httpc:request(delete, {?BASE_URL ++ Path ++ "?" ++ url_encode(Params), []}, [], []),
    mochijson:decode(JSON).

request(Path, Method, Params, ContentType, Body) ->
     Query = case Params of
             [] ->
                "";
             _ ->
                "?" ++ url_encode(Params)
            end,
    Request = httpc:request(Method, { ?BASE_URL ++ Path ++ Query
                                    , [], ContentType
                                    , Body
                                    }, [{timeout, ?HTTP_TIMEOUT}], []),
    {Return, {{_RVersion, RCode, RComment}, _RHeaders, RBody}} = Request,
    {Return, {RCode, RComment, RBody}}.

url_encode(Params) ->
    UrlEncodedParams = [yaws_api:url_encode(Elem) ++ "=" ++
                        yaws_api:url_encode(Value) ||
                        {Elem, Value} <- Params],
    string:join(UrlEncodedParams, "&").
