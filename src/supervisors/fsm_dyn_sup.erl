%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(fsm_dyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    io:format("~p: Init..\n", [?MODULE]),

    {ok, { {one_for_one, 5, 10}, []} }.

