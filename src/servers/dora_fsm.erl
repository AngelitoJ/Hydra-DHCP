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

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-export([idle/2, offer/2, bound/2]).


-define(STATE_TRACE(Id, EV, CUR, NEXT), io:format("Id: Received EV while in CUR state, going to NEXT\n", [])).


-record(st, { 
                 id          = undefined  %% Client id this FSM is managing
                ,ip          = undefined
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
   Result = lists:foldl(
                             fun bind/2              %% chain computations folding funs over the inital state
                            ,{ok, #st{}, Opts}       %% initial state
                            ,[                       %% init funs
                                 fun init_id/2
                                ,fun init_pool/2
                            ]
                        ),
    case Result of
        {ok, State, _} -> {ok, idle, State, 30000 };
        {error, Reason} -> {stop, Reason}
    end.

%% Timeout while in idle state, we stop and finish
idle(timeout, State) ->
    ?STATE_TRACE(Id, timeout, idle, stop),
    {stop, normal}.


%% Receive discover while in idle state, process request and change state accordingly.
idle({discover, MyPid, Packet}, #st{ id = Id, pools = Pools } = State) ->
    case select_pool(Pools, Id, Packet) of                                      
        {ok, AddrPid} ->                                                         %% We got suitable pool for this client
                        {ok, Addr, Opts} = pool_get_addr(AddrPid),               %% get an addres and a list of options
                        ok               = pool_mark(AddrPid, Addrs, offered),   %% mark as offered the address
                        ok               = send_offer(Mypid, Packet, Addr, Opts),%% reply to the caller with a offer message
                        ?STATE_TRACE(Id, discover, idle, offer),                 %% transition to offer state
                        {next_state, offer, State#st{ ip = Addr, options = Opts, addr_server = PoolPid }, 30000};     %% Timeout if client does not reply in 30 sec 
        {no_pool}     ->                                                         %% No pool for this client
                        ?STATE_TRACE(Id, discover, idle, stop),                  %% finish here
                        {stop, normal, State}
    end.


idle({inform, MyPid, Packet}, #st{ id = Id } = State) ->
    ?STATE_TRACE(Id, inform, idle, idle),
    {next_state, idle, State};

idle({request, init_reboot, MyPid, Packet}, #st{ id = Id } = State) ->
    case true of
        true ->
            ?STATE_TRACE(Id, request, idle, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, idle, idle),
            {next_state, idle, State}
    end;

idle(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in idle state, doing nothing",[Id,Any]),
    {next_state, idle, State}.

%% Timeout in offer state , the client has never responded we free the offered IP and go back to idle
offer(timeout, #st{ id = Id, ip = Addr, addr_server = AddrPid} = State) ->
    ok = pool_mark(AddrPid, Addr, free),
    ?STATE_TRACE(Id, timeout, offer, idle),
    {next_state, idle, State#st{ ip = undefined, addr_server = undefined , options = undefined } , 30000 };

offer({request, selecting, MyPid, Packet}, #st{ id = Id, ip = Addr, server_id = Serverid , addr_server = PoolPid } = State) ->
    case get_serverid(Packet) == Serverid of
        true ->                                            %% Client have choosen our offer
                {ok, Addr, Options} = pool_mark(PoolPid, Addr, allocated),
                ok                  = send_ack(MyPid, Packet, Addr, Options),
                ?STATE_TRACE(Id, request, offer, bound),
                {next_state, bound, State};
        false ->                                           %% Client ignored our offer
                ok = pool_mark(PoolPid, Addr, free),
                ?STATE_TRACE(Id, request, offer, stop),
                {stop, normal, State}
    end;

%%offer({ignore, MyPid, Packet}, #st{ id = Id } = State) ->
%%    ?STATE_TRACE(Id, ignore, offer, stop),
%%    {stop, ignored, State};

offer(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in offer state, doing nothing",[Id,Any]),
    {next_state, idle, State}.



bound({request, renewing, MyPid, Packet}, #st{ id = Id } = State) ->
    case true of
        true ->
            ?STATE_TRACE(Id, request, bound, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, bound, idle),
            {next_state, idle, State}
    end;

bound({request, rebinding, MyPid, Packet}, #st{ id = Id } = State) ->
    case true of
        true ->
            ?STATE_TRACE(Id, request, bound, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, bound, idle),
            {next_state, idle, State}
    end;

bound({release, MyPid, Packet}, #st{ id = Id } = State) ->
    ?STATE_TRACE(Id, release, bound, idle),
    {next_state, idle, State};

bound({decline, MyPid, Packet}, #st{ id = Id } = State) ->
    ?STATE_TRACE(Id, decline, bound, idle),
    io:format("~p: Received DECLINE while in BOUND state, going to IDLE\n", [Id]),
    {next_state, idle, State};

bound(Any, #st{ id = Id } = State) ->
    io:format("[~p]: Received this: ~p while in bound state, doing nothing",[Id,Any]),
    {next_state, idle, State}.


state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

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

%% An 'Either State a' monadic binding in Erlang..
bind(Fun, {ok, State, Opts}) when is_function(Fun) -> Fun(State, Opts);
bind(_, {error, _} = Other) -> Other.  


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
pool_select(Pools, Id, Packet) ->
    Pids = lists:fold(
                    fun({Pid, Fun}, Acc) ->
                                    case Fun(Id, Packet) of
                                        true  ->
                                                Pid|Acc];
                                        false ->
                                                Acc
                                    end, end 
                    ,[]
                    ,Pools),
    case Pids of 
        []    ->
                no_pool;          %% give no_pool to the caller
        [H|T] ->
                H                 %% Select the first Pid available 
    end.


pool_get_addr()





