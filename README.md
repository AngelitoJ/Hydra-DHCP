Hydra-DHCP
==========

Hydra, a high performance and resilient DHCP server is a research project towards a distributed and scalable 
DHCP service in Erlang/OTP for next generations IPAM scenarios.

This is the source code of an Erlang/OTP application designed to function as a DHCP (Dynamic Host Configuration Protocol)
service featuring scalability and resilence thanks to the great OTP framework capabilities.

This is work in progress, yet a rebarized "shell" is provided from day one to manage the entire OTP application as a command-line
Unix program, so for the most part you only need to follow the install instructions to get a single standalone "hydra-server" executable.

*HOLD YOUR HORSES!!*  The important bits are missing (as Apr'14) but we hope to get the bare functionality soon...
[![Build Status](https://travis-ci.org/AngelitoJ/Hydra-DHCP.svg?branch=master)](https://travis-ci.org/AngelitoJ/Hydra-DHCP) When build status is green is safe to clone the repository. We will provide also, stable releases as soon as posible.

Brief Project roadmap
======================

- Basic DHCP service
	- Simple file based data backend and SQL storage backend.
	- DHCP Fingerprinting

- High Performance DHCP Service
	- Hydra mode: Serving a pool of UDP ports (on suitable platforms with iptables like capabilities)

- Distributed DHCP service
	- Master/Slave HA schemes.
	- Multimaster HA (Mnesia)





Basic application architecture
==============================
                                       [Top Supervisor]
                                              |
          --------------------------------------------------------------------------------------------
         |                        |                      |                        |                   |
    [Supervisor]            [Supervisor]           [Supervisor]             [Supervisor]         [Supervisor]
         |                        |                 |    |                        |                   |
         |                        |    Pid?         |    |                        |               [Console]
         |                        |     --->[FSM cache] [*]                       |                   |
	[UDP_SRV_SRV]                 |    |                 |                        |              [Table Heir]
	     |                  [MIDDLEMAN_SRV]          [DORA_FSM]             [ADDR_POOL_SRV]
	     |                        |                      |                        |
	      --UDP binaries--> [MIDDLEMAN_SRV]          [DORA_FSM]             [ADDR_POOL_SRV]
	                              |                      |                        |
                                  |                  [DORA_FSM]             [ADDR_POOL_SRV]
	                              |                      |                        |
	                               --Erlang Terms--> [DORA_FSM]             [ADDR_POOL_SRV]
	                                                     |                        |
	                                                     |                        |
                                                          --Erlang Terms--> [ADDR_POOL_SRV]

    [*] Simple_one_for_one Supervisor

	1. UDP_DRV_SRV
		- One per UDP port served (currently on UDP port), dispatch UDP messages between network hosts and erlang processes.
	2. MIDDLEMAN_SRV
		- At least one per core, transform UDP message to erlang terms and viceversa, and talks to DORA finite state machines.  
	3. DORA_FSM
		- One per active client, manage the entire DHCP exchange state machine (a.k.a DORA) and talks to pool servers to get leases.
	4. ADDR_POOL_SRV
		- One per addr pool configured, manages client leases and pool policies.

	5. Miscelaneous servers.
		- Console service: frontend offering services to print all the various structures on the application.
		- Table heir:  Responsible for ets tables used across the various workers.



SYSTEM REQUIREMENTS
===================

	1. Base Operating System
		- Unix-like compatible operating systems
			- Currently tested on Mac OS X 10.8.5 (development platform)

	2. Erlang environment
		- Currently tested on Erlang R16B03-1


INSTALL
=======

	1. Get Erlang/OTP.
	2. Get rebar.
		$ git clone git://github.com/rebar/rebar.git
		$ cd rebar
		$ ./bootstrap
		Recompile: src/getopt
		...
		Recompile: src/rebar_utils
		==> rebar (compile)

		move the result rebar file to somewhere in yout $PATH
	3. Clone this repository.
	4. Build the application.
		- cd Hydra-DHCP
		- rebar clean
		- rebar compile
		- rebar escriptize
	5. Run the server
		$ ./dhcp_server

		bash-3.2$ ./dhcp_server --help

		Hydra, a high performance and resilent Erlang DHCP server.
		Copyright: 2014 Angel J. Alvarez Miguel
		Version: 0.1

		Usage: ./dhcp_server [-D <pool_dir>] [-P <procs>] [-V] [-v] [-d <debug>] [-?] [-M <middleman_pool_factor>] [-U <udp_port>]

		-D, --pooldir		Pool data directory.
		-P, --cores			Number of workers (default 2*core).
		-V, --version		Show software version.
		-v, --verbose		Show all actions performed.
		-d, --debug			Show debug info.
		-?, --help			Show this help.
		-M, --middleman		Use 1, 2 or 4 middlemen (packet processors) per core
		-U, --udp			UDP listening port.

  		Remember you need root privileges to open port 67, instead you can try something like this:

  		$ ./dhcp_server --udp 2000

  		so you dont need to be root... do you? 

  		$ sudo ./dhcp_service

  		Once the server start, you should see something like this

  		bash-3.2$ ./dhcp_server --udp 9000
		executing user profile in HOME/.erlang

		Hydra, a high performance and resilent Erlang DHCP server.
		Copyright: 2014 Angel J. Alvarez Miguel
		Version: 0.1

		dhcp_server_sup: Init, I got 5 children to spawn..
		ets_master_srv: Init..
		[addr_pool_sup] Initiating, got 2 address pools to setup
		[addr_pool_srv_1]: Initiating pool from file ./data/pool2.dat..
			POOL {name,"Universidad2"}
			POOL {range,{10,10,0,2},{10,10,1,253}}
			POOL {options,[{lease_time,3600},
               				{renewal_time,1800},
               				{rebinding_time,3000},
               				{subnet_mask,{255,255,254,0}},
               				{dns_server,{192,168,1,1}},
               				{domain_name,"uah.es"},
               				{router,{10,10,1,254}}]}
		[addr_pool_srv_2]: Initiating pool from file ./data/pool1.dat..
			POOL {name,"Universidad"}
			POOL {range,{192,168,1,2},{192,168,1,253}}
			POOL {options,[{lease_time,3600},
               				{renewal_time,1800},
               				{rebinding_time,3000},
               				{subnet_mask,{255,255,255,0}},
               				{broadcast_address,{192,168,1,255}},
               				{dns_server,{192,168,1,1}},
               				{domain_name,"uah.es"},
               				{router,{192,168,1,254}}]}
		fsm_cache_sup: Init..
		fsm_cache_srv: Init..
		fsm_dyn_sup: Init..
		middleman_sup: Init.. I got 4 children to spawn
		middleman_srv_1: Init..
		middleman_srv_2: Init..
		middleman_srv_3: Init..
		middleman_srv_4: Init..
		network_sup: Init..
		udp_driver_srv: Init..
		udp_driver_srv: I got 4 middleman pids..
		udp_driver_srv: Opening UPD port 9000..
		Application started...

		....

		[MAIN] Application Timeout
		Application stopped...




Authors
=======

- Angel J. Alvarez Miguel <angeljalvarezmiguel at gmail dot com>



References
==========

- "Erlang Programming", Cesarini, Thompson, 2009, O’Reilly. 
- "IP ADDRESS MANAGEMENT Principles and Practice", Rooney, 2011, IEEE Press.
- RFC 2131, 6361
