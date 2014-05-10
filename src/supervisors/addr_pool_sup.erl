%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(addr_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, option_specs/0, check_pool_dir/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

option_specs() ->
    {    
        [{pool_dir,$U,"pooldir",string,"Pool data directory."}]          %% options spec to request a udp port on the cmd-line
        ,[fun check_pool_dir/1]                                         %% fun to check udp_port supplied by users.
    }.

%% Check valid pool dir (and check pool files inside)
-spec check_pool_dir([any()]) -> [any()].
check_pool_dir(Opts) ->
    Dir        = proplists:get_value(datadir,Opts),

    Acc = filelib:fold_files(Dir
                                ,"pool*.dat"                                       %% Look for pool files
                                ,false                                             %% Not recursively
                                ,fun(File,{Bool,List}) -> 
                                                    case filelib:is_file(File) of  %% check pool file is valid (its fake check )
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
    io:format("~p: Init with Args: ~w\n", [?MODULE,Opts]),

    {ok, { {one_for_one, 5, 10}, []} }.

