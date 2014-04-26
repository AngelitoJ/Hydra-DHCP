%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(middleman_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% DHCP related functions
-include("dhcp_messages.hrl").

-record(middleman_state, {
                            id = undefined
                          }).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
	Id = proplists:get_value(who_you_are,Opts), 
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Id = proplists:get_value(who_you_are,Args), 
	io:format("~p: Init with Args: ~w\n", [Id,Args]),
    NewState = #middleman_state{ id = Id },
    {ok, NewState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% Handle a posible DHCP message
handle_cast({dhcp, Scope, Packet}, #middleman_state{id = Id} = State) ->
      io:format("~p: msg received!! ~p\n",[Id, binary_to_term(Packet)]),
      %% Decode the packet and propagate scope info, just to help the FSM to make up its mind.
      
    {noreply, State};

%% Handle a binary coded erlang term
handle_cast({udp, Scope, Packet}, #middleman_state{id = Id} = State) ->
      io:format("~p: msg received!! ~p\n",[Id, binary_to_term(Packet)]),
    {noreply, State};

%% Handle any unknow message
handle_cast(Msg, #middleman_state{id = Id} = State) ->
      io:format("~p: msg received!! ~p\n I must die!!\n",[Id, Msg]),
    {stop, i_dont_like_these_msgs, State}.

handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

