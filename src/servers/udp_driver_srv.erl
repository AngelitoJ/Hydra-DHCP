%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(udp_driver_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(driver_state,{ 
                         socket    = undefined  %% Socket servicing the UDP messages
                        ,pool = {[],[]}         %% pool of Middlemen available in a round-robin fashion 
                      }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, option_specs/0, check_valid_port/1]).

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
        [{udp_port,$U,"udp",{integer,67},"UDP listening port."}]          %% options spec to request a udp port on the cmd-line
        ,[fun check_valid_port/1]                                         %% fun to check udp_port supplied by users.
    }.

-spec check_valid_port([any()]) -> [any()].
check_valid_port(Opts) ->
    Port = proplists:get_value(udp_port,Opts,67),                          %% Get the UDP port requested. (67 is the default). 
    case (Port > 0) and (Port < 65535) of                                  %% Is requested port between bounds?
        true ->  Opts;                                                     %% Yes , do no touch anything
		    false -> {error, {invalid_port, Port}}                                 %% No, signal the user 
    end.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  io:format("~p: Init with Args: ~w\n", [?MODULE,Args]),

  Pool      = discover_and_setup_pool(),
  Socket    = setup_udp_port(Args),

  {ok, #driver_state{ socket = Socket, pool = Pool } }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle an UDP message from DHCP clients (68) and handle it out to a middleman process
handle_info({udp, Socket, _IP, 68, Packet}, #driver_state{ pool = Pool} = State) ->
    io:format("Received UDP packet: ~p \n", [Packet]),
    case get_next_middleman(Pool) of
        {nothing,_} ->
            io:format("~p: my middleman pool is empty , discarding received UDP packet: ~p \n", [?MODULE,Packet]),
            gen_udp:close(Socket),
            {stop, empty_pool, State};
        {Pid,NewPool} ->
            gen_server:cast(Pid,{dhcp_packet, Packet}),
            {noreply, State#driver_state{ pool = NewPool} }
    end;

%% Handle an UDP message from other clients and handle it out to a middleman process
handle_info({udp, Socket, _IP, _SrcPort, Packet}, #driver_state{ pool = Pool} = State) ->
    io:format("Received UDP packet: ~p \n", [Packet]),
    case get_next_middleman(Pool) of
        {nothing,_} ->
            io:format("~p: my middleman pool is empty , discarding received UDP packet: ~p \n", [?MODULE,Packet]),
            gen_udp:close(Socket),
            {stop, empty_pool, State};
        {Pid,NewPool} ->
            gen_server:cast(Pid,{generic_packet, Packet}),
            {noreply, State#driver_state{ pool = NewPool} }
    end;

%% Handle a DOWN message from a diying middlemen and delete it drom the pool list.
handle_info({'DOWN', _, process, Pid, Reason}, #driver_state{ pool = Pool} = State) ->
    io:format("~p: Hey! middleman ~p terminated with reason: ~p..\n", [?MODULE,Pid,Reason]),
    NewState = State#driver_state{ pool = delete_faulty_middleman(Pool, Pid) },
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

discover_and_setup_pool() ->
  MiddlemanList = [ Child || {_, Child, _, _} <- supervisor:which_children(middleman_sup)],
  lists:foreach(fun (Pid) -> erlang:monitor(process,Pid) end, MiddlemanList),
  io:format("~p: I got ~p middleman pids..\n", [?MODULE,length(MiddlemanList)]),
  {[],MiddlemanList}.


%% get a middleman Pid in round-robin fashion or nothing if no available Pids
get_next_middleman({[],[]} = M) ->           %% No Pids currently available sorry
  {nothing, M};   

get_next_middleman({Used, []}) ->            %% Recycle the Used List and try again
  get_next_middleman({[],Used});

get_next_middleman({Used, [Next|Rest]}) ->   %% Return the next Pid and cycle the list 
  {Next, {[Next|Used], Rest}}.

%% delete a faulty Pid from the available list of middleman Pids
delete_faulty_middleman({Used,Rest},Pid) ->
  {Used -- [Pid], Rest -- [Pid]}.



setup_udp_port(Opts) ->
  Port = proplists:get_value(udp_port,Opts),               %% Get the UDP port requested. (67 is the default). 
  io:format("~p: Opening UPD port ~p..\n", [?MODULE,Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, inet, {broadcast, true}, {reuseaddr, true}]),
  Socket.

