%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(middleman_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,option_specs/0,check_middleman_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Type, Mod, Args), {Id, {Mod, start_link, [Args]}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).            

option_specs() ->
    {    
        [{middleman_pool_factor,$M,"middleman",{integer,1}
                         ,"Use 1, 2 or 4 middlemen (packet processors) per core"}]          %% options spec to set the pool of middlemen coders/decoders
        ,[fun check_middleman_pool/1]                                    %% fun to check the value supplied by users.
    }.

-spec check_middleman_pool([any()]) -> [any()].
check_middleman_pool(Opts) ->
    Schedulers = erlang:system_info(schedulers_online),
    PoolFactor = proplists:get_value(middleman_pool_factor,Opts,1),      %% Get the UDP port requested. (67 is the default). 
    case (PoolFactor > 0) and (PoolFactor < 5) of                         %% Is requested port between bounds?
        true ->                                                           %% Ok, compute a suitable pool and makeup Options
        	PoolSize = Schedulers * PoolFactor,
        	[{middleman_pool, PoolSize} | proplists:delete(middleman_pool_factor,Opts)];
		false -> {error, {invalid_middleman_pool_factor, PoolFactor}}                            %% No, signal the user 
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    %% Build a list of children upon user request
    PoolSize     = proplists:get_value(middleman_pool,Opts),
    ChildrenSpec = [ ?CHILD(X, worker, middleman_srv, [{who_you_are,srv_id(X)}|Opts]) || X <- lists:seq(1,PoolSize) ],

    io:format("~p: Init.. I got ~p children to spawn\n", [?MODULE,length(ChildrenSpec)]),

    {ok, { {one_for_one, 5, 10}, ChildrenSpec } }.

%% Make a new atom to name a middlemen server
srv_id(Id) ->
	Server_name = "middleman_srv_" ++ integer_to_list(Id),
	list_to_atom(Server_name).







