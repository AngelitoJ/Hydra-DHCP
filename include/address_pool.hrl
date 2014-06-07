%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-record(address, {                              %% An address record represents this basic allocation unit
                     ip           = undefined   %% IP address
                    ,status       = undefined   %% Status = AVAILABLE | OFFERED | ALLOCATED | EXPIRED..
                    ,lease        = undefined   %% lease info if this IP is allocated
                    ,options      = undefined   %% Options to this allocation
                }).

-record(lease, {                                %% A Lease record describes a DHCP lease
                     clientid                   %% Client Id holding the lease
                    ,ip                         %% Address leased
                    ,expires                    %% Expiration timer
                }).