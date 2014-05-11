%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(addr_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, option_specs/0, check_pool_dir/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Mod, Args), {I, {Mod, start_link, [Args]}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

option_specs() ->
    {    
        [{pool_dir,$D,"pooldir",string,"Pool data directory."}]          %% options spec to request a udp port on the cmd-line
        ,[fun check_pool_dir/1]                                         %% fun to check udp_port supplied by users.
    }.

%% Check valid pool dir (and check pool files inside)
-spec check_pool_dir([any()]) -> [any()].
check_pool_dir(Opts) ->
    Dir        = proplists:get_value(pool_dir,Opts,"./data"),
    Acc = filelib:fold_files(Dir
                                ,".*"                                               %% Look for pool files
                                ,false                                              %% Not recursively
                                ,fun(File,{Bool,List}) ->
                                                    case filelib:is_file(File) of   %% check pool file is valid (its fake check )
                                                        true -> {Bool,[File|List]};
                                                        false -> {false,File}
                                                    end
                                                end
                                ,{true,[]}
                                ),
    case Acc of
        {true, []}       -> {error, {"No pool files found!",[]}};
        {true, Pools}    -> [{pool_files, Pools }|Opts];
        {false, Invalid} -> {error, {"Invalid Pools file: ", Invalid }}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    case proplists:get_value(pool_files,Opts,undefined) of
    	undefined -> 
    				{error,no_pools};
    	Pools     ->
    				ChildrenSpec = [ ?CHILD(X, worker, addr_pool_srv, [{who_you_are,srv_id(X)}]) 
    								|| X <- lists:seq(1,length(Pools)) ],
    				io:format("[~p] Initiating, got ~p address pools to setup\n", [?MODULE,length(Pools)]),
    				{ok, { {one_for_one, 5, 10}, ChildrenSpec} }
    end.

%% Make a new atom to name a middlemen server
srv_id(Id) ->
	Server_name = "addr_pool_srv_" ++ integer_to_list(Id),
	list_to_atom(Server_name).



