%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP api module
%% @end
%%%-------------------------------------------------------------------
-module(uce_smtp).

-include("uce_smtp_mime.hrl").

%% API
-export([start/2
         ,send/1
         ,send/2
         ,send/3
         ,send/5
         ,mailq/0]).

start(_Host, _Opts) ->
    application:start(uce_smtp).

%%====================================================================
%% API
%%====================================================================

send(Msg= #mime_msg{}) ->
    send(uce_smtp_mime:from(Msg),
         uce_smtp_mime:to(Msg),
         uce_smtp_mime:encode(Msg)).

send(To, Msg) ->
    send(undefined, To, Msg).

send(undefined, To, Msg) ->
    From = uce_config:get_local_option(smtp_default_from),
    send(From, To, Msg);
send(From, To, Message) ->
    {Host, Port} = uce_config:get_local_option(smtp_host),
    MX = case uce_smtp_app:need_ssl(Port) of
             true -> {Host, Port, new_ssl, uce_config:get_local_option(smtp_login)};
             false -> {Host, Port, gen_tcp, no_login}
         end,
    Ehlo = uce_config:get_local_option(smtp_default_ehlo),
    send(MX, Ehlo, From, To, Message).

send(MX, Ehlo, From, To, Msg) ->
    uce_smtp_client:send(MX, Ehlo, From, To, Msg).

mailq() ->
    supervisor:which_children(uce_smtp_sup).

%%====================================================================
%% Internal functions
%%====================================================================

