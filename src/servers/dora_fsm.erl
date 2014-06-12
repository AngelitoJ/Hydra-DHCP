%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(dora_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([idle/2, offer/2, bound/2]).


-define(STATE_TRACE(Id, EV, CUR, NEXT), io:format("Id: Received EV while in CUR state, going to NEXT\n", [])).

-include("dhcp_messages.hrl").

-record(st, { 
                 id          = undefined  %% Client id this FSM is managing
                ,addr        = undefined
                ,options     = undefined
                ,addr_server = undefined
                ,server_id   = undefined
                ,pools       = []         %% a list of funs used to select the right pool
                }).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) when is_list(Opts) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Opts) when is_list(Opts) ->
    Result = sequence(
                        {ok, #st{}, Opts}
                        ,[
                             fun init_id/2
                            ,fun init_pool/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            {ok, idle, NewState, 30000 };
        {error, Reason} ->
                            {stop, Reason}
    end.

%% Timeout while in idle state, we stop and finish
idle(timeout, State) ->
    ?STATE_TRACE(Id, timeout, idle, stop),
    {stop, normal, State};


%% Receive discover while in idle state, process request and change state accordingly.
idle({discover, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun select_pool/2
                            ,fun pool_reserve_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            ok = send_ack(NewState),
                            ?STATE_TRACE(Id, discover, idle, offer),          %% transition to offer state
                            {next_state, offer, NewState, 30000};                %% timeout if client does not reply in 30 sec
        {error, no_pool} ->
                            ?STATE_TRACE(Id, discover, idle, stop),           %% finish here
                            {stop, normal, State};
        {error, Reason} ->
                            ?STATE_TRACE(Id, discover, idle, stop),           %% 
                            {stop, Reason, State}
    end;



idle({inform, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                         {ok, State, Packet}
                        ,[
                            fun pool_get_conf/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            send_ack(NewState),
                            ?STATE_TRACE(Id, inform, idle, idle),
                            {next_state, idle, State, 30000};
        {error, Reason}   ->
                            send_nack(State),
                            ?STATE_TRACE(Id, inform, idle, idle),
                            {next_state, idle, State, 30000}
    end;

idle({request, init_reboot, MyPid, Packet}, #st{ id = Id, pools = Pools} = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun select_pool/2
                            ,fun pool_check_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            send_ack(NewState),
                            ?STATE_TRACE(Id, request, idle, bound),
                            {next_state, bound, NewState, 30000};
        {error, no_pool} ->
                            send_nack(State, no_pool),
                            ?STATE_TRACE(Id, request, idle, idle),   
                            {next_state, idle, State};
        {error, no_lease} ->
                            send_nack(State, no_lease),
                            ?STATE_TRACE(Id, request, idle, idle),
                            {next_state, idle, State}
    end;

idle(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in idle state, doing nothing",[Id,Any]),
    {next_state, idle, State}.

%% Timeout in offer state , the client has never responded, so we free the offered IP and go back to idle
offer(timeout, #st{ id = Id, addr = Addr, addr_server = AddrPid} = State) ->
    Result = sequence(
                        {ok, State, []}         %% initial state
                        ,[                       
                             fun pool_free_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            ?STATE_TRACE(Id, timeout, offer, idle),
                            {next_state, bound, NewState, 30000};
        {error, Reason} ->
                            ?STATE_TRACE(Id, timeout, offer, stop),
                            {stop, Reason, State}
    end;

offer({request, selecting, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun is_server_selected/2
                            ,fun pool_allocate_addr/2
                        ]),
    case Result of
        {ok, NewState, _}    ->                                           %% Allocation was ok send ACK to client
                                send_ack(NewState, MyPid),
                                ?STATE_TRACE(Id, request, offer, bound),
                                {next_state, bound, NewState};
        {error, no_selected} ->                                           %% Client choosed other server for this request
                                ?STATE_TRACE(Id, ignore, offer, offer),
                                {next_state, offer, State, 1000};         %% setup timer ito force address release on timeout
        {error, Reason}      ->                                           %% Something went wrong, siognal to client
                                send_nack(State, MyPid),
                                ?STATE_TRACE(Id, request, offer, stop),
                                {next_state, idle, State}
    end;

offer(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in offer state, doing nothing",[Id,Any]),
    {next_state, idle, State}.



bound({request, renewing, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun pool_extend_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            send_ack(NewState, MyPid),
                            ?STATE_TRACE(Id, request, bound, bound),
                            {next_state, bound, NewState};
        {error, Reason}   ->
                            send_nack(State, MyPid),
                            ?STATE_TRACE(Id, request, bound, idle),
                            {next_state, idle, State}
    end;

bound({request, rebinding, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun is_server_selected/2
                            ,fun pool_extend_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            send_ack(NewState, MyPid),
                            ?STATE_TRACE(Id, request, bound, bound),
                            {next_state, bound, NewState};
        {error, Reason}   ->
                            send_nack(State, MyPid),
                            ?STATE_TRACE(Id, request, bound, idle),
                            {next_state, idle, State}
    end;

bound({release, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun pool_release_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            ?STATE_TRACE(Id, release, bound, idle),
                            {next_state, idle, NewState};
        {error, Reason} ->
                            ?STATE_TRACE(Id, release, bound, stop),
                            {stop, Reason, State}
    end;

bound({decline, MyPid, Packet}, #st{ id = Id } = State) ->
    Result = sequence(
                        {ok, State, Packet}         %% initial state
                        ,[                       
                             fun pool_decline_addr/2
                        ]),
    case Result of
        {ok, NewState, _} ->
                            ?STATE_TRACE(Id, decline, bound, idle),
                            {next_state, idle, NewState};
        {error, Reason} ->
                            ?STATE_TRACE(Id, decline, bound, stop),
                            {stop, Reason, State}
    end;

bound(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in bound state, doing nothing",[Id,Any]),
    {next_state, bound, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #st{id = Id} = State) ->
    io:format("[~p]: DORA for client ~w terminated\n", [Id,Id]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


bind(Fun, {ok, State, Opts}) when is_function(Fun) -> Fun(State, Opts);
bind(_, {error, _} = Other) -> Other.  

sequence(Initial, Computations) ->
    lists:foldl(fun bind/2, Initial, Computations).

init_id(State, Opts) ->
    case proplists:is_defined(client_id,Opts) of
        true  ->
                Id = proplists:get_value(client_id,Opts), 
                io:format("[~p]: DORA Init for client ~w\n", [Id,Id]),
                {ok, #st{ id = Id}, Opts};
        false ->
                {error, no_client_id}
    end.

%% Gather pools server pids and selection funs
init_pool(#st{ id = Id} = State, Opts) ->
    case supervisor:which_children(addr_pool_sup) of
        [Head |Tail] ->
                        PoolInfo = lists:foldl(
                                        fun({_, Pid, _, _}, Acc) -> 
                                                            erlang:monitor(process, Pid),
                                                            case gen_server:call(Pid, selection_fun) of
                                                                Fun when is_function(Fun) -> 
                                                                                        [{Pid, Fun}| Acc];
                                                                _                         -> Acc
                                                            end end
                                        ,[]
                                        ,[Head|Tail] ),
                        io:format("[~p]: I got ~p address pool server funs..\n", [Id, length(PoolInfo)]),
                        {ok, State#st{ pools = PoolInfo }, Opts};
        _             ->
                        {error, no_pool_servers }
    end.


%% Get suitable pool server for this request.
select_pool({ pools = Pools, id = Id, request = Packet} = State, Opts) ->
    Pids = lists:fold(
                    fun({Pid, Fun}, Acc) ->
                                    case Fun(Id, Packet) of
                                        true  ->
                                                [Pid|Acc];
                                        false ->
                                                Acc
                                    end end 
                    ,[]
                    ,Pools),
    case Pids of 
        []    ->
                {error, no_pool};                                    %% give no_pool to the caller
        [H|T] ->
                {ok, State#st{ addr_server = H}, Opts}                 %% Select the first Pid available 
    end.

%% Try to check if the requested address is still suitable fot this client
pool_check_addr(#st{ id = Id, addr_server = PoolPid } = State, #dhcp_packet{ rqaddr = Addr } = Packet) -> 
    case gen_server:call(PoolPid,{check, Id, Addr}) of
        {ok, Addr, Opts} ->
                            {ok, State#st{ addr = Addr, options = Opts }, Packet};
        {error, Reason}  ->
                            {error, Reason}
    end.
%% Try reserve an address at the pool server, or signal the error
pool_reserve_addr(#st{ id = Id, addr_server = PoolPid } = State, Aux) -> 
    case gen_server:call(PoolPid, {reserve,Id}) of
        {ok, Addr, Opts} ->
                            {ok, State#st{ addr = Addr, options = Opts }, Aux};
        false            ->
                            {error, no_pool}
    end.

%% Try to allocate an address at the pool server or signal the error
pool_allocate_addr(#st{ id = Id, addr_server = PoolPid, addr = Addr } = State, Aux) ->
    case gen_server:call(PoolPid, {allocate, Id, Addr}) of
        {ok, Addr, Opts} ->
                            {ok, State, Aux};
        {error, Reason}  ->
                            {error, Reason}
    end.

%% Try to extend a lease for some address
pool_extend_addr(#st{ id = Id, addr_server = PoolPid, addr = Addr } = State, Aux) ->
    case gen_server:call(PoolPid, {extent, Id, Addr}) of
        {ok, Addr, Opts} ->
                            {ok, State, Aux};
        {error, Reason}  ->
                            {error, Reason}
    end.

%% Decline is just a special case for free
pool_decline_addr(State, Aux) -> 
    pool_free_addr(State, decline). 
%% Release is just a special case for free
pool_release_addr(State, Aux) -> 
    pool_free_addr(State, release). 


%% Try to free an address at the pool server or signal the error
pool_free_addr(#st{ id = Id, addr_server = PoolPid, addr = Addr } = State, Aux) ->
    case gen_server:call(PoolPid, {free, Id, Addr}) of
        ok              ->
                            {ok, State#st{ addr = undefined, options = undefined}, Aux};
        {error, Reason} ->
                            {error, Reason}
    end.

pool_get_conf(#st{ id = Id, addr_server = PoolPid, addr = Addr } = State, Aux) ->
    case gen_server:call(PoolPid, {conf, Id}) of
        {ok, Opts}      ->
                            {ok, State, Aux};
        {error, Reason} ->
                            {error, Reason}
    end.

%% Test whether the server_id of the packet is equal to this FSM server_id
is_server_selected(State, Packet) -> 
    case get_server_id(Packet) == State#st.server_id of
        true ->
                {ok, State, Packet};
        false ->
                {error, no_selected}
    end.

get_server_id(_) -> undefined.
select_pool(_,_,_) -> undefined.
send_offer(_,_,_,_) -> undefined.
send_ack(_) -> undefined.
send_nack(_) -> undefined.
send_ack(_,_) -> undefined.
send_nack(_,_) -> undefined.
get_serverid(_) -> undefined.
get_request_addr(_) -> undefined.
pool_mark(_,_,_) -> undefined.



