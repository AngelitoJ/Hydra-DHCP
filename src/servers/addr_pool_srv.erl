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

-record(subnet,	{                               %% a subnet is the basic bloc of Ip addresses allocation
					 network                    %% base network of this subnet
					,netmask                    %% netmask to generate the subnet
					,range                      %% range of IPs available in this subnet
					,excluded                   %% excluded Ips in this subnet
					,options                    %% Options available in this subnet
				}).

-record(host, {                                 %% A host record describe a client Id holding a certain IP address
					 id                         %% Client Id
					,ip                         %% Address allocated to this client Id
				}).

-record(address, {                              %% An address record represents this basic allocation unit
					 ip                         %% IP address
					,status                     %% Status = AVAILABLE | OFFERED | ALLOCATED | EXPIRED..
					,timer = undefined          %% Expiration timer
					,options = undefined        %% Options to this allocation
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
	State                               = load_pool(#st{ id = Id, filepath = PoolFile}),

	io:format("[~p]: Initiating pool from file ~ts..\n", [Id,PoolFile]),

    {ok, State}.


handle_call({allocate, Clientid}, _From, State) ->
    {reply, ok, State};
handle_call({reserve, Clientid}, _From, State) ->
    {reply, ok, State};
handle_call({extend, Clientid}, _From, State) ->
    {reply, ok, State};
handle_call({decline, Clientid}, _From, State) ->
    {reply, ok, State};
handle_call({release, Clientid}, _From, State) ->
    {reply, ok, State};
handle_call({verify, Clientid} , _From, State) ->
    {reply, ok, State};
handle_call({info, Clientid}, _From, State) ->
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
		{simplepool, PoolOpts} -> lists:foldl(
												 fun init_pool/2
												,State
												,PoolOpts
											);
		_ -> error("pool type not supported")
	end.

%% Make a new addresses table (or recover it from the table heir)
init_pool({name, Name}, #st{ id = Id } = State) ->
	Table = list_to_atom(Name),
	io:format("[~p]: Creating ETS pool named ~p..\n", [Id,Table]),
	Tid = case gen_server:call(ets_master_srv, {get, Table}) of
		  {ok, Value}       -> Value;
		  {not_found, Pid}  -> ets:new(Table, [
												 public
												,{keypos, #address.ip}
												,{heir, Pid, {name, Table}}
											])
	      end,
	State#st{ pool = Tid };

%% Populate the ETS table with free addresses 		
init_pool({range, Start, End}, #st{ id = Id, pool = Tid } = State) ->
	io:format("[~p]: Populationg Pool from ~p to ~p..\n", [Id,Start,End]),
	populate(Tid, free, Start, End),
	State;

%% record the pool client options
init_pool({options, Options}, #st{ id = Id } = State) ->
	lists:foreach(
					fun(I) -> io:format("[~p]: option: ~p\n",[Id, I]) end,
					Options
				),
	State#st{ options = Options };

init_pool(Tuple,#st{ id = Id } = State) ->
	io:format("[~p]: Unknow Pool setting ~p:\n", [Id,Tuple]),
	State.

%% Populate the given table with address records on a given status, use explicit tail recursion to avoid building a large list
populate(Tid, Status, Addr, Addr) ->
	io:format("Addres: ~p Status: ~p\n",[Addr,Status]),
	ets:insert(Tid, #address{
								 ip     = Addr
								,status = Status
							});
populate(Tid, Status, Addr, End) ->
	io:format("Addres: ~p Status: ~p\n",[Addr,Status]),
	ets:insert(Tid, #address{
								 ip     = Addr
								,status = Status
							}),
	populate(Tid, Status, ipv4_succ(Addr), End).







