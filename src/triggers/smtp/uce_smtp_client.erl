%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Simple one-shot client using esmtp_fsm.
%% @end
%%%-------------------------------------------------------------------
-module(uce_smtp_client).

%% API
-export([send/5
         ,start_link/5
         ,init/5
         ,sendemail/5]).

%%====================================================================
%% API
%%====================================================================

send(MX, Ehlo, From, To, Msg) when is_list(From), is_list(To) ->
    is_mx(MX),
    supervisor:start_child(uce_smtp_sup, [MX, Ehlo, From, To, Msg]).

start_link(MX, Ehlo, From, To, Msg) ->
    proc_lib:start_link(?MODULE, init, [MX, Ehlo, From, To, Msg]).

%%====================================================================
%% Internal functions
%%====================================================================

init({Host,Port},Ehlo,From,To,Msg) ->
    init({Host,Port,tcp},Ehlo,From,To,Msg);
init(MX,Ehlo,From,To,Msg) ->
    proc_lib:init_ack({ok, self()}),
    sendemail(MX,Ehlo,From,To,Msg).

sendemail({Host, Port, SSL},Ehlo,From,To,Msg) ->
    {ok, S0} = uce_smtp_sock:connect(Host, Port, SSL),
    {ok, S1, {220, _Banner}} = uce_smtp_sock:read_response(S0),
    {ok, S2, {250, _Msg}} = uce_smtp_sock:command(S1, {ehlo, Ehlo}),
	User = uce_config:get_global_option(smtp_login),
	Pass = uce_config:get_global_option(smtp_password),
    AuthS = case {User,Pass} of
				{"", ""} -> S2;
				{undefined, undefined} -> S2;
                _ ->
                    {ok, S3, {334, _}} = uce_smtp_sock:command(S2, {auth, "PLAIN"}),
                    {ok, S4, {235, _}} = uce_smtp_sock:command(S3, {auth_plain, User, Pass}),
                    S4
            end,
    {ok, S10, {250, _}} = uce_smtp_sock:command(AuthS, {mail_from, From}),
    {ok, S11, {250, _}} = uce_smtp_sock:command(S10, {rcpt_to, To}),
    {ok, S12, {250, _}} = uce_smtp_sock:send_data(S11, Msg),
    ok = uce_smtp_sock:close(S12).

is_mx({_Host,Port}) when is_integer(Port) -> true;
is_mx({_Host,Port,new_ssl}) when is_integer(Port) -> true;
is_mx({_Host,Port,gen_tcp}) when is_integer(Port) -> true.
