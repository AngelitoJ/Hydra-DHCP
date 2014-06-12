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
remove_pid({Used, Rest},Pid) when is_pid(Pid) ->
  case {Used -- [Pid], Rest -- [Pid]} of
    {[],[]}       -> empty;
    {List1,List2} -> {List1,List2}
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pool_test_() -> { inparallel, [
                    %% empty new
                   {"pool new",?_assertEqual(empty
                                    ,new())}
                    %% empty list new
                  ,?_assertEqual(empty
                                    ,new([]))
                    %% non empty list new
                  ,{"pool new2",?_assertEqual({[], [one,two,three]}
                                    ,new([one,two,three]))}
                    %% pool get next
                  ,{"pool get_next1",?_assertEqual({one, {[one],[two,three]}}
                                    ,get_next(new([one,two,three])) )}
                    %% pool get next with list recycling
                  ,{"pool get_next2",?_assertEqual({one, {[one],[two,three]}}
                                    ,get_next( {[one,two,three], []} ) )}
                    %% pool remove
                  ,{"pool remove_pid", ?_assertEqual({[one], [three]}, remove_pid({[one,self()], [three]},self()))}
                ]}.

-endif.
