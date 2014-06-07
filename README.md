Hydra-DHCP
==========

Hydra, a high performance and resilient DHCP server is a research project towards a distributed and scalable 
DHCP service in Erlang/OTP for next generations IPAM scenarios.

This is the source code of an Erlang/OTP application designed to function as a DHCP (Dynamic Host Configuration Protocol)
service featuring scalability and resilence thanks to the great OTP framework capabilities.

This is work in progress, yet a rebarized "shell" is provided from day one to manage the entire OTP application as a command-line
Unix program, so for the most part you only need to follow the install instructions to get a single standalone "hydra-server" executable.


Project status
===============

[![Build Status](https://travis-ci.org/AngelitoJ/Hydra-DHCP.svg?branch=master)](https://travis-ci.org/AngelitoJ/Hydra-DHCP) 

The repository is registered on Travis-ci, so a green status means all software compile and unit tests (if any) went fine.

We will provide a stable release on reaching v 1.0.

Component status legend:
SETUP: The component basic lyout is done, some bare functionalty but is doesn't work as designed yet.
WIP  : The component has several aspects implemented, and its receiving new features.
DONE : The component is finished[1] and seems to perform as designed but can contain obvious bugs.
QA   : The component is subject to major unit testing


- Escript fronted                 : DONE
	- Command-line parsing , and complete statup and shutdoen of the bundle OTP application. 
- Cross module get-opt            : DONE
	- Gathering of options specs across all bundled modules.
- Main OTP tree                   : DONE
	- Simple (non distributed) applicaction startup with config data provided from escript frontend.
- Console server                  : SETUP
- Table master                    : DONE
	- Simple inheritance from process deaths , and service of ETS table identifiers to any process.
- UDP Driver                      : SETUP
	- UDP port configurable via command-line.
	- Iface and server ID configuration in progress.
- Middleman DHCP encoder / decoder: SETUP
- DORA Cache manager              : SETUP
- DORA DHCP State machine         : SETUP
- Address Pool Servers             : WIP  
	- 'Simple pool type' this type manages a simple range of IP's, and a list of options usimg a ETS table and a DETS table to persist lease information. Simple format suitable for file:consult() usage
	-{ pooldata,{simplepool, [
						 {name, "Universidad"}
        				,{range, {192,168,1,2}, {192,168,1,253}}
						,{options, [ 
					 		         {lease_time, 3600}
          					        ,{renewal_time, 1800}
          					        ,{rebinding_time, 3000}
          					        ,{subnet_mask, {255, 255, 255, 0}}
          					        ,{broadcast_address, {192, 168, 1, 255}}
          					        ,{dns_server, {192, 168, 1, 1}}
          					        ,{domain_name, "uah.es"}
          					        ,{router, {192, 168, 1, 254}}
          					         ]} ]}
    - Init functions terminated ( file parse, load and state/tables population) 




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
		- At least one per core, transform UDP message into erlang terms and viceversa, and talks (async) to DORA finite state machines.
	3. DORA_FSM
		- One per active client, manage the entire DHCP exchange state machine (a.k.a DORA) and talks (sync) to pool servers
		 to get leases.
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

		bash-3.2$ ./dhcp_server --timeout 10 --udp 9000

		Hydra, a high performance and resilent Erlang DHCP server.
		Copyright: 2014 Angel J. Alvarez Miguel
		Version: 0.1

		top_sup: Init, I got 5 children to spawn..
		misc_sup: Init..
		[console_srv]: Init... 
		ets_master_srv: Init..
		[addr_pool_sup] Initiating, got 2 address pools to setup
		[addr_pool_srv_1]: Creating ETS pool named 'Universidad2'..
		[addr_pool_srv_1]: Populating Pool from {10,10,0,2} to {10,10,1,253}..
		[addr_pool_srv_1]: option: {lease_time,3600}
		[addr_pool_srv_1]: option: {renewal_time,1800}
		[addr_pool_srv_1]: option: {rebinding_time,3000}
		[addr_pool_srv_1]: option: {subnet_mask,{255,255,254,0}}
		[addr_pool_srv_1]: option: {dns_server,{192,168,1,1}}
		[addr_pool_srv_1]: option: {domain_name,"uah.es"}
		[addr_pool_srv_1]: option: {router,{10,10,1,254}}
		[addr_pool_srv_1]: Initiating pool from file ./data/pool2.dat..
		[addr_pool_srv_2]: Creating ETS pool named 'Universidad'..
		[addr_pool_srv_2]: Populating Pool from {192,168,1,2} to {192,168,1,253}..
		[addr_pool_srv_2]: option: {lease_time,3600}
		[addr_pool_srv_2]: option: {renewal_time,1800}
		[addr_pool_srv_2]: option: {rebinding_time,3000}
		[addr_pool_srv_2]: option: {subnet_mask,{255,255,255,0}}
		[addr_pool_srv_2]: option: {broadcast_address,{192,168,1,255}}
		[addr_pool_srv_2]: option: {dns_server,{192,168,1,1}}
		[addr_pool_srv_2]: option: {domain_name,"uah.es"}
		[addr_pool_srv_2]: option: {router,{192,168,1,254}}
		[addr_pool_srv_2]: Initiating pool from file ./data/pool1.dat..
		dora_cache_sup: Init..
		dora_cache_srv: Init..
		dora_dyn_sup: Init..
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
