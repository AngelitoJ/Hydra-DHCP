%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel




-module(dora_cache_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(st,{
				 children = undefined %% clientid to pid cache
				}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
	io:format("~p: Init..\n", [?MODULE]),
	State = #st{ children = cache:new() },
    {ok, State}.

%% Lookup a FSM pid by its Client ID (and spawn a new child if not found)
handle_call({get_pid, Clientid}, _From, #st{ children = Children } = State) ->
    case cache:look_by_id(Children, Clientid) of
        %% Found, just reply with the requested value
        {ok, ClientPid} -> {reply, ClientPid, State};

        %% Not found, we need to spawn a new client FSM, the new child will get a clientid to know what to to 
        error -> case supervisor:start_child(dora_dyn_sup, [{clientid = Clientid}]) of
                    %% Ok Monitor the new child and update dicts before replying the new Pid
                    {ok, Pid} -> erlang:monitor(process, Pid),
                                {reply, Pid, State#st{ children = cache:insert(Children, Clientid, Pid)} };

                    %% Something went wrong, just tell the caller
                    {error, Error} -> {reply, {error, Error}, State};

                    %% Something went even worst, just tell the caller
                    Other -> {reply, {error, Other}, State}
                end
    end.

%%handle_call(_Request, _From, State) ->
%%    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle a DOWN message from a died child and remove it drom the cache.
handle_info({'DOWN', _, process, Pid, Reason}, #st{children = Children} = State) ->
    io:format("[~p]: Hey! client FSM ~p terminated with reason: ~p..\n", [?MODULE,Pid,Reason]),
    {noreply,  #st{ children = cache:remove_by_pid(Children, Pid) }};

handle_info(Info, State) ->
    io:format("[~p]: Hey! I received this: ~p\n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------











