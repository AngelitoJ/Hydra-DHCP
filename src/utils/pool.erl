%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(pool).

-export([new/0, new/1, get_next/1, remove_pid/2]).


%% make a new pool of Pids
new()                        -> new([]).
new([])                      -> empty;
new(Pids) when is_list(Pids) -> {[],Pids}.

%% POOL: get next Pid in round-robin fashion or nothing if no available Pids
get_next(empty)               -> empty;   
get_next({Used, []})          -> get_next({[],Used});      %% Recycle the used list and try again
get_next({Used, [Next|Rest]}) -> {Next, {[Next|Used], Rest}}.         %% Return the next Pid and cycle the list 


%% delete a faulty Pid from the available list of Pids and return the new pool or 'empty'
remove_pid(empty, Pid) when is_pid(Pid) ->
    empty;
remove_pid({Used, Rest},Pid) when is_pid(Pid) ->
  case {Used -- [Pid], Rest -- [Pid]} of
    {[],[]}       -> empty;
    {List1,List2} -> {List1,List2}
  end.

