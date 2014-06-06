%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

%% A simple {pid,id} cache

-module(cache).

-export([new/0, insert/3, lookup_by_pid/2, lookup_by_id/2, remove_by_pid/2]).

-record(pidcache,{
                 pids = undefined        %% id to pid dict
                ,ids  = undefined        %% pid to id dict
                }).



new() -> #pidcache{ 
                     pids = dict:new()
                    ,ids = dict:new()
                    }.

insert(#pidcache{ pids = Pids, ids = Ids }, Id, Pid) ->
    #pidcache{ 
                 pids = dict:store(Id, Pid, Pids)
                ,ids  = dict:store(Pid, Id, Ids) }.


lookup_by_id(#pidcache{ pids = Pids }, Id) -> dict:find(Id, Pids).   %% id -> pid

lookup_by_pid(#pidcache{ ids = Ids }, Pid) -> dict:find(Pid, Ids).   %% pid -> id


remove_by_pid(#pidcache{ pids= Pids, ids = Ids } = Cache, Pid) ->
    case dict:find(Pid, Ids) of
        {ok, Id} -> #pidcache{ 
                                     pids = dict:erase(Id, Pid, Pids)
                                    ,ids  = dict:erase(Pid, Id, Ids) };
        error          -> Cache
    end.





