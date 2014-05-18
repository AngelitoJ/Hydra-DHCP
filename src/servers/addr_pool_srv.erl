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


-record(pool_state, {
						 id        = undefined
						,filepath  = undefined
						,data      = []         %% A property list containing pool attributes
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
	Data                                = load_pool(PoolFile),
    NewState                            = #pool_state{ id = Id, filepath = PoolFile, data = Data},

	io:format("[~p]: Initiating pool from file ~ts..\n", [Id,PoolFile]),

	lists:foreach(fun(I) -> io:format("POOL ~p\n",[I]) end,
					Data),

    {ok, NewState}.


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

load_pool(File) ->
	{ok, [{pooldata,PoolData}|_]} = file:consult(File),
	case PoolData of
		{simplepool, PoolOpts} -> PoolOpts;
		_ -> error("pool type not supported")
	end.
