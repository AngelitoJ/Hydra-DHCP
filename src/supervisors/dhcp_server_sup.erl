%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(dhcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, infinity, Type, [I]}).

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
                      ?CHILD(addr_pool_sup, supervisor, Opts)  %% Address pool servers supervisor
                     ,?CHILD(fsm_sup,       supervisor, Opts)  %% DORA finite state machines supervisors
                     ,?CHILD(middleman_sup, supervisor, Opts)  %% Middleman pool (coding,decoding and fingerprinting) supervisor
                     ,?CHILD(network_sup,   supervisor, Opts)  %% Network components supervisor
                    ],

    {ok, { {one_for_one, 5, 10},
    							ChildrenSpec } }.

