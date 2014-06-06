%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(dora_dyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDWRK(I, Args), {I, {I, start_link, [Args]}, temporary, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    io:format("[~p]: Init..\n", [?MODULE]),

    {ok, { {simple_one_for_one, 5, 10}, [?CHILDWRK(dora_fsm, Opts)]} }.

