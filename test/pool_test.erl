%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(pool_test).



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pool_test_() -> { inparallel, [
                    %% empty new
                   {"pool new, no args",?_assertEqual(empty
                                    ,pool:new())}
                    %% empty list new
                  ,{"pool new, empty list"
                                    ,?_assertEqual(empty, pool:new([]))}
                    %% non empty list new
                  ,{"pool new with args",?_assertEqual({[], [one,two,three]}, pool:new([one,two,three]))}
                    %% pool get next
                  ,{"pool get_next"
                        ,?_assertEqual({one, {[one],[two,three]}}, pool:get_next(pool:new([one,two,three])) )}
                    %% pool get next with list recycling
                  ,{"pool get_next, recycing pool"
                        ,?_assertEqual({one, {[one],[two,three]}}, pool:get_next( {[one,two,three], []} ) )}
                    %% pool remove
                  ,{"pool remove_pid"
                        ,?_assertEqual({[one], [three]}, pool:remove_pid({[one,self()], [three]},self()))}
                  ,{"pool remove_pid, empty pool"
                        , ?_assertEqual(empty, pool:remove_pid(pool:new(),self()))}
                ]}.

-endif.
