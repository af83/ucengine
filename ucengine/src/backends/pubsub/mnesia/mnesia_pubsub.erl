%%
%%  U.C.Engine - Unified Collaboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(mnesia_pubsub).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-export([init/1,
         start_link/0,
         publish/2,
         subscribe/6,
         unsubscribe/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("uce.hrl").

-record(uce_mnesia_pubsub, {pid, domain, location, type, from, parent}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

publish(Domain, #uce_event{location=Location, type=Type} = Event) ->
    ?COUNTER('pubsub:publish'),
    %% Publish on the domain
    gen_server:call(?MODULE, {publish, Domain, "", Type, Event}),
    case Location of
        "" ->
            ok;
        Location ->
            gen_server:call(?MODULE, {publish, Domain, Location, Type, Event})
    end.

subscribe(Pid, Domain, Location, From, "", Parent) ->
    subscribe(Pid, Domain, Location, From, [""], Parent);
subscribe(Pid, Domain, Location, From, Types, Parent) ->
    ?COUNTER('pubsub:suscribe'),
    [gen_server:cast(?MODULE, {subscribe,
                               Domain,
                               Location,
                               From,
                               Type,
                               Parent,
                               Pid}) || Type <- Types].

unsubscribe(Pid) ->
    ?COUNTER('pubsub:unsubscribe'),
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%
% Gen server callbacks
%

init([]) ->
    mnesia:create_table(uce_mnesia_pubsub,
                        [{ram_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, uce_mnesia_pubsub)}]),
    {ok, {}}.

handle_call({publish, Domain, Location, Type, #uce_event{from=From, parent=Parent} = Event}, _From, State) ->
    Return =
        case get_subscribers(Domain, Location, Type, From, Parent) of
            {error, Reason} ->
                {error, Reason};
            Subscribers ->
                [Subscriber#uce_mnesia_pubsub.pid ! {event, Event} || Subscriber <- Subscribers],
                ok
        end,
    {reply, Return, State}.

handle_cast({subscribe, Domain, Location, From, Type, Parent, Pid}, State) ->
    mnesia:transaction(fun() ->
                               mnesia:write(#uce_mnesia_pubsub{pid=Pid,
                                                               domain=Domain,
                                                               location=Location,
                                                               type=Type,
                                                               from=From,
                                                               parent=Parent})
                       end),
    {noreply, State};
handle_cast({unsubscribe, Pid}, State) ->
    mnesia:transaction(fun() ->
                               mnesia:delete({uce_mnesia_pubsub, Pid})
                       end),
    {noreply, State}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.

%
% Private functions
%

get_subscribers(Domain, Location, Type, From, Parent) ->
    Transaction = fun() ->
                          Query = qlc:q([Subscriber || #uce_mnesia_pubsub{domain=SubscribedDomain,
                                                                          location=SubscribedLocation,
                                                                          type=SubscribedType,
                                                                          from=SubscribedFrom,
                                                                          parent=SubscribedParent} = Subscriber
                                                           <- mnesia:table(uce_mnesia_pubsub),
                                                (SubscribedDomain == Domain) andalso
                                                (SubscribedLocation == Location) andalso
                                                ((SubscribedType == Type) or (SubscribedType == "")) andalso
                                                ((SubscribedFrom == From) or (SubscribedFrom == "")) andalso
                                                ((SubscribedParent == Parent) or (SubscribedParent == ""))
                                        ]),
                          qlc:eval(Query)
                  end,
    case mnesia:transaction(Transaction) of
        {aborted, _} ->
            {error, bad_parameters};
        {atomic, Subscribers} ->
            Subscribers
    end.
