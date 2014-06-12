%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(address_tools_test).



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("address_tools.hrl").

pool_test_() -> { inparallel, [
                    %% empty new
                   {"get_network"
                        ,?_assertEqual(
                                         {192, 168, 0, 0}
                                        ,get_network({192, 168, 23, 234},{255, 255, 0, 0})
                                        )}
                ]}.

-endif.
