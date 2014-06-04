%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(addr_pool_srv).
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

-include("address_tools.hrl").

-record(address, {                              %% An address record represents this basic allocation unit
                     ip                         %% IP address
                    ,status                     %% Status = AVAILABLE | OFFERED | ALLOCATED | EXPIRED..
                    ,clientid                   %% ID of client holding the address if any.
                    ,options      = undefined   %% Options to this allocation
                }).

-record(lease, {                                %% A Lease record describes a DHCP lease
                     clientid                   %% Client Id holding the lease
                    ,ip                         %% Address leased
                    ,expires                    %% Expiration timer
                }).

-record(st, {                                   %% Address Pool server state record 
                         id        = undefined  %% Id of this pool
                        ,filepath  = undefined  %% Path to pool data
                        ,options   = undefined  %% A property list containing pool attributes
                        ,pool      = undefined  %% A ets table holding address allocations.
                        ,leases    = undefined  %% A dets table holding leases. 
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

init(Opts) ->
    Id                                  = proplists:get_value(who_you_are,Opts),
    PoolFile                            = proplists:get_value(pool_file,Opts),

    io:format("[~p]: Initiating pool from file ~ts..\n", [Id,PoolFile]),
    State                               = load_pool(#st{ id = Id, filepath = PoolFile}),
    
    {ok, State}.


%% Look for a suitable free address for the client (client can request one) and mark it as offered.
handle_call({reserve, ClientId, RequestedIP}, _From, State) ->
    Result = case select_address(State, ClientId, RequestedIP) of
        {ok, Address}   -> reserve_address(State, Address, ClientId);
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, State};

%% Mark an address as allocated. Address must be in offered state 
handle_call({allocate, ClientId, RequestedIP}, _From, State) ->
    Result = case pool_lookup(State, RequestedIP) of
        {offered, Address} -> allocate_address(State, Address, ClientId);
        {_, _Address}      -> {error, "Address is not offered."};
        not_found          -> {error, "Address was not found."}
    end,
{reply, Result, State};


handle_call({extend, _ClientId}, _From, State) ->
    {reply, ok, State};
handle_call({decline, _ClientId}, _From, State) ->
    {reply, ok, State};
handle_call({release, _ClientId}, _From, State) ->
    {reply, ok, State};
handle_call({verify, _ClientId} , _From, State) ->
    {reply, ok, State};
handle_call({info, _ClientId}, _From, State) ->
    {reply, ok, State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

load_pool(#st{ filepath = File } = State) ->
    {ok, [{pooldata,PoolData}|_]} = file:consult(File),
    case PoolData of
        {simplepool, PoolOpts} -> load_simple_pool(State, PoolOpts);
        _ -> error("pool type not supported")
    end.

%% Load a simple pool from a property list 
load_simple_pool(State, Opts) ->
    {name, Name} = proplists:lookup(name, Opts),     
    PoolName     = list_to_atom(Name),                     %% Atom for the ETS table name
    LeasesFile   = Name ++ ".leases",                      %% String for the DETS table file
    LeasesName   = list_to_atom(LeasesFile),               %% Atom for the DETS table name
    PoolOpts     = [
                     {poolname, PoolName}                   %% Name of the ETS pool
                    ,{leasesname, LeasesName, LeasesFile}   %% declare a leases file attached tp this pool
                    ] ++ proplists:delete(name, Opts),      %% delete old name property

    %% Fold over the property list setting up state for every known option
    NewState = lists:foldl(fun state_init/2, State ,PoolOpts),

    %% Now we have to transfer active leases from the leases table to the pool table
    leases_to_pool(NewState),
    NewState.


%% Create a new ETS table for the address pool (or recover it from the table heir) 
state_init({poolname, Name}, #st{ id = Id } = State) ->

    io:format("[~p]: Creating ETS pool named ~p..\n", [Id,Name]),
    %% Ask Table heir if table already exists, create a new one if needed and set the heir
    Tidpool = case gen_server:call(ets_master_srv, {get, Name}) of     
        {ok, Value}       -> Value;
        {not_found, Pid}  -> ets:new(Name, [                            
                                                 public
                                                ,{keypos, #address.ip}
                                                ,{heir, Pid, {name, Name}}
                                            ])
        end,
    State#st{ pool = Tidpool };

%% Open or recreate a DETS file to persist lease information
state_init({leasesname, Name, File}, #st{ id = Id } = State) -> 

    io:format("[~p]: Creating DETS leases table named ~p..\n", [Id, Name]),
    %% Open lease file or die..
    {ok, Tidleases} = dets:open_file(Name, [
                                                  {keypos, #lease.clientid}
                                                 ,{file, File}
                                            ]),

    State#st{ leases =  Tidleases };


%% Populate the ETS table with free addresses         
state_init({range, Start, End}, #st{ id = Id, pool = Tid } = State) ->
    io:format("[~p]: Populating pool from ~p to ~p..\n", [Id,Start,End]),
    pool_populate(
                     Tid
                    ,fun(IP) -> %%io:format("[~p]: Population pool with address ~p\n",[Id, IP]),
                                #address{ ip = IP, status = free , clientid = undefined} end
                    ,Start
                    ,End
                    ),
    State;

%% record the pool client options
state_init({options, Options}, #st{ id = Id } = State) ->
    lists:foreach(
                    fun(I) -> io:format("[~p]: option: ~p\n",[Id, I]) end,
                    Options
                ),
    State#st{ options = Options };

state_init(Tuple,#st{ id = Id } = State) ->
    io:format("[~p]: Unknow Pool setting ~p:\n", [Id,Tuple]),
    State.

%% Lookup a IP address and tag it accordingly free | offered | not_found
pool_lookup(State, Ip) ->
    case ets:lookup(State#st.pool, Ip) of
        [Address] when Address#address.status == free    -> {free, Address};
        [Address] when Address#address.status == offered -> {offered, Address};
        _                                                -> not_found
    end.
%% Insert or replace an addres object in the pool
pool_insert(State, Address) when is_record(Address, address) ->
    ets:insert(State#st.pool, Address).

%% Populate the given table with address records on a given status (provided by a fun)
%% use explicit tail recursion to avoid building a large list
pool_populate(Tid, Fun, Addr, Addr) when is_function(Fun) ->
    ets:insert(Tid, Fun(Addr));

pool_populate(Tid, Fun, Addr, End) ->
    ets:insert(Tid, Fun(Addr)),
    pool_populate(Tid, Fun, ipv4_succ(Addr), End).

%% Lookup a client lease and tag it accordingly ok | expired | not_found
leases_lookup(State, Id, Now) ->
    case dets:lookup(State#st.leases, Id) of
        [Lease] when Lease#lease.expires > Now -> {ok, Lease};
        [Lease]                                -> {expired, Lease};         
        []                                     -> not_found
    end.

%% Insert a new lease
leases_insert(State, Lease) when is_record(Lease, lease) ->
    ok = dets:insert(State#st.leases, Lease).

%% delete a client lease
leases_remove(State, Id) -> 
    ok = dets:delete(State#st.leases, Id).

%% Transfer active leases to pool folding over the dets table to avoid collecting large results
leases_to_pool(#st{ id = Id, options = Options, leases = Tidleases } = State) ->
    Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
    dets:foldl(
                fun(Lease, Acc) -> case Lease#lease.expires > Now of
                                   %% Populate the pool with the address extraacted from the list
                                   true  -> io:format("[~p]: Transferring client ~p lease to pool.\n",[Id,Lease#lease.clientid]),
                                            pool_insert(State, #address{ 
                                                             ip       = Lease#lease.ip
                                                            ,options  = Options
                                                            ,clientid = Lease#lease.clientid 
                                                            }),
                                    Acc;
                                    false -> Acc
                                    end
                end
                ,[]
                ,Tidleases).


%% Try to select the first free IP address available
select_address(#st{ pool = Tid , options = Options }, _ClientId, {0, 0, 0, 0}) ->
    case ets:match(Tid, #address{ ip = '$1', status = free }, 1) of
        [Addr] -> {ok, #address{ ip = Addr, status = free, options = Options}};
        []     -> {error, "No address is available for this request"}
    end;

%% Try to select the client requested IP if posible.
select_address(#st{ options = Options } = State, ClientId, RequestedIP) ->
    Now = calendar:datetime_to_gregorian_seconds({date(), time()}),

    case leases_lookup(State, ClientId, Now) of
        %% There is a active lease, we use it ignoring the client requested IP
        {ok, Lease}      -> {ok, #address{ip = Lease#lease.ip, options = Options}};

        %% There is a expired lease, we have to check the pool                                                                
        {expired, Lease} -> case pool_lookup(State, Lease#lease.ip) of
                            %% The address is free we can reuse it anyway
                            {free, Address}    -> {ok, Address#address{options = Options}};
                            %% The address is not free , we remove the lease and try again
                            _                  -> leases_remove(State, ClientId),
                                                  select_address(State, ClientId, RequestedIP)
                            end;
        %% There is no lease we try to get a new address from the pool                
        not_found        -> case pool_lookup(State, RequestedIP) of
                            %% the address is free we use it
                            {free, Address}    -> {ok, Address#address{ options = Options}};
                            %% the address is offered 
                            {offered, _Address} -> {error, "Address is offered"};
                            %% no address found we try any address free
                            not_found          -> select_address(State, ClientId, {0,0,0,0})
                           end
    end.

%% Mark an address as offered and commit to leases file (should'nt be committed by now, I have to check this out)
reserve_address(State, Address, ClientId) when is_record(Address, address) ->
    pool_insert(State, Address#address{status = offered}),
    
    Now     = calendar:datetime_to_gregorian_seconds({date(), time()}),
    Expires = Now + 300, %% 5 minutes ? I have to check this out
    Lease   = #lease{ 
                         clientid = ClientId
                        ,ip       = Address#address.ip
                        ,expires  = Expires},
    
    leases_insert(State, Lease),


    {ok, Address#address.ip, Address#address.options}.

%% Mark an IP address as allocated to a Client.
allocate_address(State, ClientId, RequestedIP, Options) ->
    allocate_address(State, #address{ip = RequestedIP, options = Options}, ClientId).

allocate_address(State, Address, ClientId) ->
    case proplists:get_value(leasetime, Address#address.options, not_found) of
    not_found -> {error, "Lease time not configured."};
    {lease_time, LeaseTime} ->
        Now     = calendar:datetime_to_gregorian_seconds({date(), time()}),
        Expires = Now + LeaseTime,
        Lease   = #lease{ clientid = ClientId, ip = Address#address.ip, expires = Expires},
        leases_insert(State, Lease),
        pool_insert(State, Address#address{status = allocated}),
        {ok, Address#address.ip, Address#address.options }
    end.











