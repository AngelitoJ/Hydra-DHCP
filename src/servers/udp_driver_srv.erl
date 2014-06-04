%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(udp_driver_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(driver_state,{ 
                         socket  = undefined  %% Socket servicing the UDP messages
                        ,pool    = empty      %%  {Used,Rest} | empty, pool of Middlemen available in a round-robin fashion 
                      }).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, option_specs/0, check_iface/1, check_valid_port/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

option_specs() ->
    {    
        [
           {udp_iface, $I, "iface", string, "Listen on iface."}
          ,{udp_port,$U,"udp",{integer,67},"UDP listening port."}]          %% options spec to request a udp port on the cmd-line
        ,[
           fun check_iface/1
          ,fun check_valid_port/1]                                         %% fun to check udp_port supplied by users.
    }.

-spec check_valid_port([any()]) -> [any()].
check_valid_port(Opts) ->
    Port = proplists:get_value(udp_port,Opts,67),                          %% Get the UDP port requested. (67 is the default). 
    case (Port > 0) and (Port < 65535) of                                  %% Is requested port between bounds?
        true  ->  Opts;                                                    %% Yes , do no touch anything
		    false -> {error, {invalid_port, Port}}                             %% No, signal the user 
    end.

-spec check_iface([any()]) -> [any()].
check_iface(Opts) ->
  Iface = proplists:get_value(udp_iface,Opts,undefined),                %% Get the server id requested. 
  case Iface of
    undefined -> Opts;
    _ -> {error, {invalid_udp_iface, Iface}}                                
    end.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
  io:format("~p: Init..\n", [?MODULE]),

  {ok, Pool}      = discover_and_setup_pool(),                                   
  Socket          = setup_udp_port(Opts),

  {ok, #driver_state{ socket = Socket, pool = Pool } }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle an UDP message from DHCP clients (68) and handle it out to a middleman process
%% When we ran out of middleman Pids, we shoult ask for more, right now we quit and let supervior instantiate a new driver
handle_info({udp, _Socket, Ip, 68, Packet}, #driver_state{ pool = Pool} = State) ->
    io:format("Received UDP packet: ~p \n", [Packet]),
    Msg = case Ip of 
      {0,0,0,0} -> {dhcp, broadcast, Packet};
      _         -> {dhcp, unicast, Packet}
    end,
    NewPool = send_to_pool(Msg,Pool),
    {noreply, State#driver_state{ pool = NewPool} };

%% Handle an UDP message from other clients and handle it out to a middleman process
%% When we ran out of middleman Pids, we shoult ask for more, right now we quit and let supervior instantiate a new driver
handle_info({udp, _Socket, Ip, _SrcPort, Packet}, #driver_state{ pool = Pool} = State) ->
    io:format("Received UDP packet: ~p \n", [Packet]),
    Msg = case Ip of 
      {0,0,0,0} -> {udp, broadcast, Packet};
      _         -> {udp, unicast, Packet}
    end,
    NewPool = send_to_pool(Msg,Pool),
    {noreply, State#driver_state{ pool = NewPool} };

%% Handle a DOWN message from a diying middlemen and delete it drom the pool list.
handle_info({'DOWN', _, process, Pid, Reason}, #driver_state{ pool = Pool} = State) ->
    io:format("~p: Hey! middleman ~p terminated with reason: ~p..\n", [?MODULE,Pid,Reason]),
    NewState = State#driver_state{ pool = pool:remove_pid(Pool, Pid) },
    {noreply,  NewState };

handle_info(Info, State) ->
	  io:format("~p Info msg received: ~p\n",[?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Ask middleman supervisor for their children a store the list 
discover_and_setup_pool() ->
  case supervisor:which_children(middleman_sup) of
    [Head |Tail] ->
      Pids = lists:map(fun({_, Pid, _, _}) -> erlang:monitor(process, Pid), Pid end, [Head|Tail]),
      io:format("~p: I got ~p middleman pids..\n", [?MODULE,length(Pids)]),
      {ok, pool:new(Pids)};
    _ ->
      {error, pool:new()}
  end.

%% send a msg to a Pid from the Pool, recycle the pool if empty, return the new pool
send_to_pool(Msg,Pool) ->
  case pool:get_next(Pool) of
    empty ->                                     %% the pool is empty!
      io:format("~p: my middleman pool is empty, i'm getting a new one from the supervisor\n", [?MODULE]),
      {ok, NewPool} = discover_and_setup_pool(),       %% get a new pool
      send_to_pool(Msg,NewPool);                       %% try again!
    {Pid, NewPool} ->                                  %% Ok we have at least one Pid
      gen_server:cast(Pid,Msg),                        %% Send the Msg
      NewPool
  end.
    

%% Get UDP port from Options and setup socket
setup_udp_port(Opts) ->
  Port = proplists:get_value(udp_port,Opts),               %% Get the UDP port requested. 
  io:format("~p: Opening UPD port ~p..\n", [?MODULE,Port]),
  {ok, Socket} = gen_udp:open(Port, [
                                       binary              %% send packets as binaries instead of lists
                                      ,inet                %% make a IPv4 socket
                                      ,{broadcast, true}   %% Allow bradcast
                                      ,{reuseaddr, true}   %% Allows local reuse of port numbers
                                    ]),
  Socket.



