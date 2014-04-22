Hydra-DHCP
==========

Hydra, a high performance and resilent DHCP server is a research project towards a distributed and scalable 
DHCP service coded in Erlang/OTP for next generation IPAM scenarios.

This is the source code of an Erlang/OTP application designed to function as a DHCP (Dynamic Host Configuration Protocol)
service featuring scalability and resilence thanks to the great OTP framework capabilities.

This is work in progress, yet a rebarized "shell" is provided from day one to manage the entire OTP application as a command-line
Unix program, so for the most part you only need to follow the install instructions to get a single standalone "hydra-server" executable.

*HOLD YOUR HORSES!!*  The important bits are missing (as Apr'14) but we hope to get the bare functionality soon...


Brief Project roadmap
======================

- Basic DHCP service
	- Simple file based data backend and SQL storage backend.
	- Master/Slave HA schemes.

- Distributed DHCP service
	- Multimaster data backend (Mnesia)


Basic application arquitecture
==============================

	[UDP_SRV_SRV] <--UDP binaries--> [MIDDLEMAN_SRV] <--Erlang Terms--> [DORA_FSM] <--Erlang Terms--> [ADDR_POOL_SRV]

	1. UDP_DRV_SRV
		- One per UDP port served (currently only UDP 67), dispatch UDP messages between network hosts and erlang processes.
	2. MIDDLEMAN_SRV
		- At least one per core, transform UDP message to erlang terms and viceversa, and talks to DORA finite state machines.  
	3. DORA_FSM
		- One per active client, manage the entire DHCP exchange state machine (a.k.a DORA) and talks to pool servers to get leases.
	4. ADDR_POOL_SRV
		- One per addr pool configured, manages client leases and pool policies.

SYSTEM REQUIREMENTS
===================

	1. Base Operating System
		- Unix-like compatible operating systems
			- Currently tested on Mac OS X 10.8.5 (development platform)

	2. Erlang enviroment
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
		Hydra DHCP Server, a high performance and resilent Erlang DHCP server.
		Copyright: 2014 Angel J. Alvarez Miguel
		Version: 0.1

		Usage: ./dhcp_server [-P <procs>] [-V] [-v] [-d <debug>] [-?] [-D <middleman_pool_factor>] [-U <udp_port>]

  		-P, --cores		Number of workers (default 2*core).
  		-V, --version		Show software version.
  		-v, --verbose		Show all actions performed.
  		-d, --debug		Show debug info.
  		-?, --help		Show this help.
  		-D, --decoders	Use <n> packet processors per core
  		-U, --udp		UDP listening port.



Authors
=======

- Angel J. Alvarez Miguel <angeljalvarezmiguel at gmail dot com>



References
==========

- "Erlang Programming", Cesarini, Thompson, 2009, O’Reilly. 
- "IP ADDRESS MANAGEMENT Principles and Practice", Rooney, 2011, IEEE Press.
