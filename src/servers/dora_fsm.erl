%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(dora_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-export([idle/2, offer/2, bound/2]).

-record(fsm_state, { id = undefined }).
-define(STATE_TRACE(Id, EV, CUR, NEXT), io:format("Id: Received EV while in CUR state, going to NEXT\n", [])).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) when is_list(Opts) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [{who_you_are,test_fsm}|Opts], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Opts) when is_list(Opts) ->
    Id = proplists:get_value(who_you_are,Opts), 
    io:format("~p: Init with Args: ~w\n", [Id,Opts]),
    {ok, idle, #fsm_state{ id = Id} }.


idle({discover, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, discover, idle, offer),
    {next_state, offer, State, 30000};

idle({inform, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, inform, idle, idle),
    {next_state, idle, State};

idle({request, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    case true of
        true ->
            ?STATE_TRACE(Id, request, idle, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, idle, idle),
            {next_state, idle, State}
    end.

offer(timeout, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, timeout, offer, idle),
    {next_state, idle, State};

offer({request, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    case true of
        true -> 
            ?STATE_TRACE(Id, request, offer, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, offer, idle),
            {next_state, idle, State}
    end;

offer({ignore, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, ignore, offer, stop),
    {stop, ignored, State}.

bound({request, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    case true of
        true ->
            ?STATE_TRACE(Id, request, bound, bound),
            {next_state, bound, State};
        false ->
            ?STATE_TRACE(Id, request, bound, idle),
            {next_state, idle, State}
    end;

bound({release, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, release, bound, idle),
    {next_state, idle, State};

bound({decline, MyPid, Packet}, #fsm_state{ id = Id } = State) ->
    ?STATE_TRACE(Id, decline, bound, idle),
    io:format("~p: Received DECLINE while in BOUND state, going to IDLE\n", [Id]),
    {next_state, idle, State}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

