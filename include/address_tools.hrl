%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


%% IPv4 conversion functions <<A:8,B:8,C:8,D:8>> <--> {A,B,C,D}
ipv4_to_tuple(<<A:8, B:8, C:8, D:8>>) -> {A, B, C, D}.
tuple_to_ipv4({A,B,C,D})              ->  <<A:8, B:8, C:8, D:8>>.

%% MAC 802.3 conversion functions <<A:8,B:8,C:8,D:8,E:8,F:8, _/binary>> <--> {A,B,C,D,E,F}
mac_to_tuple(<<A:8, B:8, C:8, D:8, E:8, F:8, _/binary>>) -> {A, B, C, D, E, F}.
tuple_to_mac({A, B, C, D, E, F}) ->  <<A:8, B:8, C:8, D:8, E:8, F:8, 0:80>>.

%% get the next ipv4 avoiding especial cases 0 and 255 
ipv4_succ({A, 254, 254, 254}) -> ipv4_succ({A+1, 0, 0, 0});
ipv4_succ({A, B, 254, 254})   -> ipv4_succ({A, B+1, 0, 0});
ipv4_succ({A, B, C, 254})     -> ipv4_succ({A, B, C+1, 0});
ipv4_succ({A, B, C ,D})       -> {A, B ,C ,D+1}.