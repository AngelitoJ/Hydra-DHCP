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

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("dhcp_messages.hrl").
-include("address_tools.hrl").
-include("address_pool.hrl").



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
    case proplists:is_defined(who_you_are, Opts) of
        true -> 
                Id = proplists:get_value(who_you_are,Opts),
                gen_server:start_link({local, Id}, ?MODULE, Opts, []);
        false ->
                {stop, not_id}
    end.



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
    Result = sequence(
                         {ok, #st{}, Opts}                 %% Initial empty state and opts from supervisor
                        ,[                        
                             fun init_id/2                 %% get client id we are managing
                            ,fun init_filepath/2           %% get pool config file
                            ,fun init_simple_pool/2        %% load the config file contents
                            ,fun init_table_names/2        %% seup table names and files
                            ,fun init_addrs_table/2        %% create the main table (ETS)
                            ,fun init_leases_table/2       %% open or create the leases only table (DETS)
                            ,fun init_range/2              %% populate addrs table with entries from allowed range
                            ,fun init_options/2            %% store options into state
                            ,fun init_leases/2             %% transfer active leases from DEST to ETS and remove stale entries
                        ]),

    case Result of
        {ok, State, _} -> {ok, State};
        {error, Reason, _State} -> {stop, Reason}
    end.

%% give a fun suitable to select this pool
handle_call(selection_fun, _From, State) ->
    Fun = fun(_Packet) ->
                        case true of
                            true -> true;
                            false -> false
                        end end,
    {reply, Fun,State};

%% Look for a suitable free address for the client (client can request one) and mark it as offered.
handle_call({reserve, ClientId, RequestedIP}, _From, State) ->
    Result = case select_address(State, ClientId, RequestedIP) of
        {ok, IP}        ->
                            case reserve_address(State, ClientId, IP) of
                                ok              -> {ok, IP, State#st.options};  
                                {error, Reason} -> {error, Reason}
                            end;  
        {error, Reason} ->
                            {error, Reason}
    end,
    {reply, Result, State};

%% Mark an address as allocated. Address must be in offered state 
handle_call({allocate, ClientId, RequestedIP}, _From, State) ->
    Result = case allocate_address(State, ClientId, RequestedIP) of
        ok              ->
                            {ok, RequestedIP, State#st.options};
        {error, Reason} ->
                            {error, Reason}
    end,
    {reply, Result, State};

%% Try to extent the lease on an allocated IP
handle_call({extend, ClientId, RequestedIP}, _From, State) ->
    Result = case extend_address(State, ClientId, RequestedIP) of
        ok              ->
                            {ok, RequestedIP, State#st.options};
        {error, Reason} ->
                            {error, Reason}
    end,
    {reply, Result, State};

handle_call({decline, ClientId, RequestedIP}, _From, State) ->
    Result = case decline_address(State, ClientId, RequestedIP) of
        ok              ->
                            {ok, RequestedIP, State#st.options};
        {error, Reason} ->
                            {error, Reason}
    end,
    {reply, Result, State};

handle_call({release, ClientId, RequestedIP}, _From, State) ->
    Result = case release_address(State, ClientId, RequestedIP) of
        ok              ->
                            {ok, RequestedIP, State#st.options};
        {error, Reason} ->
                            {error, Reason}
    end,
    {reply, Result, State};
handle_call({verify, _ClientId} , _From, State) ->
    {reply, ok, State};

handle_call({info, _ClientId}, _From, State) ->
    {reply, ok, State};


handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%%
bind(Fun, {ok, State, Env}) when is_function(Fun) -> Fun(State, Env);
bind(_, {error, _, _} = Other) -> Other.  

sequence(Initial, Computations) ->
    lists:foldl(fun bind/2, Initial, Computations).


%% Setup process is and pool identity 
init_id(State, Opts) ->
    case proplists:is_defined(who_you_are, Opts) of
        true -> 
                {ok, State#st{ id = proplists:get_value(who_you_are,Opts)}, Opts};
        false ->
                {error, not_id, State}
    end.

%% Setup path to pool file
init_filepath(State, Opts) ->
    case proplists:is_defined(pool_file, Opts) of
        true -> 
                {ok, State#st{ filepath = proplists:get_value(pool_file,Opts)}, Opts};
        false ->
                {error, not_pool_file, State}
    end.

%% Load pool contest from file 
init_simple_pool(#st{ id = Id } = State, Opts) ->
    case file:consult(State#st.filepath) of
        {ok, [{pooldata,{simplepool, PoolOpts}}|_]} -> 
            io:format("[~p]: Initiating pool from file ~ts..\n", [Id,State#st.filepath]),
            {ok, State, PoolOpts};
        _ -> 
            {error, unknow_pool_format, State}
    end.

%% Setup pool and table names
init_table_names(State,Opts) ->
    case proplists:is_defined(name, Opts) of
        true ->
            {name, Name} = proplists:lookup(name, Opts),
            PoolName     = list_to_atom(Name),                     %% Atom for the ETS table name
            LeasesFile   = Name ++ ".leases",                      %% String for the DETS table file
            LeasesName   = list_to_atom(LeasesFile),               %% Atom for the DETS table name
            NewOpts      = [
                             {poolname, PoolName}                   %% Name of the ETS pool
                            ,{leasesname, LeasesName, LeasesFile}   %% declare a leases file attached tp this pool
                            ] ++ proplists:delete(name, Opts),      %% delete old name property
            {ok, State, NewOpts};
        false ->
                {error, no_pool_name, State}
    end.


%% Create a new ETS table for the address pool (or recover it from the table heir) 
init_addrs_table(#st{ id = Id } = State, Opts) ->
    case proplists:is_defined(poolname,Opts) of
        true ->
                Name = proplists:get_value(poolname, Opts),
                io:format("[~p]: Creating ETS pool named ~p..\n", [Id,Name]),
                %% Ask Table heir if table already exists, create a new one if needed and set the heir
                Tidpool = case gen_server:call(ets_master_srv, {get, Name}) of     
                    {ok, Value}       ->
                                        Value;
                    {not_found, Pid}  ->
                                        ets:new(Name, [                            
                                                        public
                                                        ,{keypos, #address.ip}
                                                        ,{heir, Pid, {name, Name}}
                                                        ])
                end,
                {ok, State#st{ pool = Tidpool }, Opts};
        false ->
                {error, no_ets_name, State}
    end.

%% Open or recreate a DETS file to persist lease information
init_leases_table(#st{ id = Id} = State, Opts) ->
    case proplists:is_defined(leasesname, Opts) of
        true ->
                {leasesname, Name, File } = proplists:lookup(leasesname, Opts),
                io:format("[~p]: Creating DETS leases table named ~p..\n", [Id, Name]),
                %% Open lease file or die..
                {ok, Tidleases} = dets:open_file(Name, [
                                                         {keypos, #lease.clientid}
                                                        ,{file, File}
                                                        ]),
                {ok, State#st{ leases =  Tidleases }, Opts};
        false ->
                {error, no_dets_name, State}
    end.

%% Populate the ETS table with free addresses         
init_range(#st{id = Id, pool = Tid } = State, Opts) ->
    case proplists:is_defined(range, Opts) of
        true -> 
                {range, Start, End} = proplists:lookup(range, Opts),
                Entries = pool_populate(
                                 Tid
                                ,fun(IP) -> %%io:format("[~p]: Population pool with address ~p\n",[Id, IP]),
                                            #address{ ip = IP, status = free, lease = undefined } end
                                ,Start
                                ,End ),
                io:format("[~p]: Populating pool from ~p to ~p.. (~p entries)\n", [Id,Start,End, Entries]),
                {ok, State, Opts};
        false -> {error, no_range, State}
    end.

%% store pool client options
init_options(#st{ id = Id } = State, Opts) ->
    case proplists:is_defined(options, Opts) of
        true -> 
                Options =  proplists:get_value(options, Opts),
                lists:foreach(
                                fun(I) -> io:format("[~p]: option: ~p\n",[Id, I]) end,
                                Options ),
                {ok, State#st{ options = Options }, Opts};
        false -> 
                {error, no_pool_options, State}
    end.

%% Transfer leases to pool folding over the dets table to avoid collecting large results
init_leases(#st{ id = Id, options = Options, leases = Tidleases } = State, Opts) ->
    Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
    Entries = dets:foldl(
                fun(Lease, Acc) -> case Lease#lease.expires > Now of
                                   %% Populate the pool with the address extracted from the list
                                   true  -> 
                                            pool_update(State, #address{ 
                                                             ip       = Lease#lease.ip
                                                            ,options  = Options
                                                            ,lease    = Lease
                                                            }),
                                            Acc + 1;
                                    false ->
                                            leases_remove(State, Lease#lease.clientid),
                                            Acc
                                    end
                end
                ,0
                ,Tidleases),
    io:format("[~p]: Transferring active client leases to ETS pool (~p entries).\n",[Id, Entries]),
    {ok, State, Opts}.


%% Lookup pattern matching againt some specified address template
pool_lookup(#st{ pool = Tid }, Pattern) ->
    case ets:match(Tid, Pattern, 1) of
        [Addr] -> {ok, Addr};
        []     -> not_found
    end.

%% Replace an addres object in the pool
pool_update(State, Address) when is_record(Address, address) ->
    ets:insert(State#st.pool, Address).

%% Populate the given table with address records on a given status (provided by a fun)
%% use explicit tail recursion to avoid building a large list
pool_populate(Tid, Fun, Start, End) when is_function(Fun) ->
    pool_populate(Tid, Fun, Start, End, 1).
pool_populate(Tid, Fun, Addr, Addr, I)  ->
    ets:insert(Tid, Fun(Addr)),
    I;
pool_populate(Tid, Fun, Addr, End, I) ->
    ets:insert(Tid, Fun(Addr)),
    pool_populate(Tid, Fun, ipv4_succ(Addr), End, I+1).

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


select_address(State, ClientId, RequestedIP)   -> {error, not_implemented}.
reserve_address(State, ClientId, RequestedIP)  -> {error, not_implemented}.
allocate_address(State, ClientId, RequestedIP) -> {error, not_implemented}.
extend_address(State, ClientId, RequestedIP) -> {error, not_implemented}.
decline_address(State, ClientId, RequestedIP) -> {error, not_implemented}.
release_address(State, ClientId, RequestedIP) -> {error, not_implemented}.









