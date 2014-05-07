%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(dhcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDSUP(I, Args), {I, {I, start_link, [Args]}, permanent, infinity, supervisor, [I]}).
-define(CHILDWRK(I, Args), {I, {I, start_link, [Args]}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    io:format("~p: Init with Args: ~w\n", [?MODULE,Opts]),

    ChildrenSpec = [ 
                      ?CHILDWRK(ets_master_srv, Opts)  %% ETS master server, the heir of all ets tables, I hope it never dies..
                     ,?CHILDSUP(addr_pool_sup,  Opts)  %% Address pool servers supervisor
                     ,?CHILDSUP(fsm_cache_sup,  Opts)  %% cache and sup for finite state machines.
                     ,?CHILDSUP(middleman_sup,  Opts)  %% Middleman pool (coding,decoding and fingerprinting) supervisor
                     ,?CHILDSUP(network_sup,    Opts)  %% Network components supervisor
                    ],

    {ok, { {one_for_one, 5, 10},
    							ChildrenSpec } }.

