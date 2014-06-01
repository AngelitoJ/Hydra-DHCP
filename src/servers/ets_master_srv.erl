%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(ets_master_srv).
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

-record(st,{
			tables = []      %% dict of {Name,Tid} this process knows about
			}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Opts) ->
	io:format("~p: Init..\n", [?MODULE]),

    {ok, #st{ tables = dict:new() }}.

%% Tell a process the Tid of a table (if we know that)
handle_call({get, Name}, From, #st{ tables = Tables} = State) ->
	Result = case dict:find(Name, Tables) of
			{ok, Value} -> {ok, Value};
			error       -> {not_found, self()}
			end,
	{reply, Result, State};

%% Remove a Table in case the process wants to close it
handle_call({remove, Name}, From, #st{ tables = Tables} = State) ->	
    {reply, ok, State#st{ tables = dict:erase(Name,Tables) }};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Gain table ownership when some process screws up. Heirdata must contain the table Name
handle_info({'ETS-TRANSFER', Tid, _FromPid, {name , Name}}, #st{ tables = Tables } = State) ->
	{noreply, State#st{ tables = dict:update(Name, fun(X) -> X end, Tid, Tables)}};  

%% Failed attept to gain a table ownership because we dont know the table's name 
handle_info({'ETS-TRANSFER', _Tid, FromPid, _Whatever}, State) ->
	io:format("[TABLE MASTER]: Sorry!, process ~p died, but we can not inherit its table\n", [FromPid]),
	{noreply, State};  

handle_info(Info, State) ->
	io:format("[TABLE MASTER]: Sorry!, I received this: ~p and I dont like it...\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

