%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-record (dhcp_packet, {
						 op = 0
						,htype = 0
						,hlen = 0
						,hops = 0
						,xid = 0
						,secs = 0
						,flags = 0
						,ciaddr = {0, 0, 0, 0}
						,yiaddr = {0, 0, 0, 0}
						,siaddr = {0, 0, 0, 0}
						,giaddr = {0, 0, 0, 0}
						,chaddr = {0, 0, 0, 0, 0, 0}
						,sname = 0
						,file = 0
						,options = []
						,msg_type
						,requested_ip = {0, 0, 0, 0}
					   }).

-define(DHCP_PACKET, <<Op:8, HType:8, HLen:8, Hops:8, Xid:32, Secs:16, Flags:16/big
						, CIAddr:4/binary, YIAddr:4/binary, SIAddr:4/binary, GIAddr:4/binary, CHAddr:16/binary
						, SName:512, File:1024, 99:8, 130:8, 83:8, 99:8, Options/binary>>).

-define(DHCP_RECORD, #dhcp_packet{ op = Op, htype = HType, hlen = HLen, hops = Hops, xid = Xid, secs = Secs
						, flags = Flags, ciaddr = CIAddr, yiaddr = YIAddr, siaddr = SIAddr, giaddr = GIAddr
						, chaddr = CHAddr, sname = SName, file = File, options = DecodedOptions, msg_type = MessageType
						, requested_ip = RequestedIp }).


%% make a tagged msg suitable to be sent to a DORA gen_fsm. Tags allow easier processing in the fsm, because
%% most combinations of field in the DHCP packet will map to a diferent message.
record_to_msg(?DHCP_RECORD) -> unasigned.


%% map a DHCP binary packet into a record for easy processing.
packet_to_record(?DHCP_PACKET) ->
  DecodedOptions                      = dhcp_decode_options(Options),
  {message_type, MessageType}         = dhcp_option_search(message_type, DecodedOptions),
  {requested_ip_address, RequestedIp} = dhcp_option_search(requested_ip_address,DecodedOptions),


	Record = #dhcp_packet{
				 msg_type     = MessageType
				,requested_ip = RequestedIp
  				,op           = Op
  				,htype        = HType
  				,hlen         = HLen
  				,hops         = Hops
  				,xid          = Xid
  				,secs         = Secs
  				,flags        = Flags
  				,ciaddr       = ipv4_to_tuple(CIAddr)
  				,yiaddr       = ipv4_to_tuple(YIAddr)
  				,siaddr       = ipv4_to_tuple(SIAddr)
  				,giaddr       = ipv4_to_tuple(GIAddr)
  				,chaddr       = mac_to_tuple(CHAddr)
  				,sname        = SName
  				,file         = File
  				,options      = DecodedOptions
  							},
  {ok, Record};

packet_to_record(Unknown) ->
	{error, unknown_packet}.

%% build a DHCP binary packet out a record
record_to_packet(?DHCP_RECORD) ->
	EncodedOptions = dhcp_encode_options(DecodedOptions),
	EncodedPacket = <<Op:8, HType:8, HLen:8, Hops:8, Xid:32, Secs:16, Flags:16/big
						,(tuple_to_ipv4(CIAddr))/binary, (tuple_to_ipv4(YIAddr))/binary
						,(tuple_to_ipv4(SIAddr))/binary, (tuple_to_ipv4(GIAddr))/binary
						,(tuple_to_mac(CHAddr))/binary, SName:512, File:1024
						,<<99:8, 130:8, 83:8, 99:8>>/binary
						,EncodedOptions/binary>>,
	{ok, EncodedPacket};

record_to_packet(_) ->
	{error, unknown_record}.


%% IPv4 conversion functions <<A:8,B:8,C:8,D:8>> <--> {A,B,C,D}
ipv4_to_tuple(<<A:8, B:8, C:8, D:8>>) -> {A, B, C, D}.
tuple_to_ipv4({A,B,C,D})              ->  <<A:8, B:8, C:8, D:8>>.

%% MAC 802.3 conversion functions <<A:8,B:8,C:8,D:8,E:8,F:8, _/binary>> <--> {A,B,C,D,E,F}
mac_to_tuple(<<A:8, B:8, C:8, D:8, E:8, F:8, _/binary>>) -> {A, B, C, D, E, F}.
tuple_to_mac({A, B, C, D, E, F}) ->  <<A:8, B:8, C:8, D:8, E:8, F:8, 0:80>>.


%% map DHCP options binary to a property list 
dhcp_decode_options(Binary) -> dhcp_decode_options(Binary, []).
dhcp_decode_options(<<0:8, Rest/binary>>, []) -> dhcp_decode_options(Rest, []);
dhcp_decode_options(<<255:8, _/binary>>, Opts) -> Opts;                            % We are done?
dhcp_decode_options(<<Op:8, Len:8, Data:Len/binary, Rest/binary>>, Opts) ->        % Default
	Name  = decode_option_name(Op),
  	Value = decode_option_value(Name, Data),
  	dhcp_decode_options(Rest,[{Name, Value }|Opts]).

%% map DHCP options property list into a binary
dhcp_encode_options([{Name, Val} | Rest]) ->
	Op      = encode_option_name(Name),
	Data    = encode_option_value(Name, Val),
	Len     = size(Data),
	<<Op:8, Len:8, Data/binary, (dhcp_encode_options(Rest))/binary>>;
dhcp_encode_options([]) -> <<255:8>>.                                               % We are done!! (i guess..)


%% search for DHCP options and provide sane defaults.
dhcp_option_search(requested_ip_address,Opts) -> 
	case lists:keysearch(requested_ip_address, 1, Opts) of
		{value, Value} -> Value;
        _              -> {requested_ip_address, {0, 0, 0, 0}}
    end;
dhcp_option_search(Key,Opts) ->
	case lists:keysearch(Key, 1, Opts) of
		{value, Value} -> Value;
        Other          -> Other
    end.



%% decode a option number into a symbolic name, not all option are implemented..
decode_option_name(1) ->  subnet_mask;
decode_option_name(2) ->  time_offset;
decode_option_name(3) ->  router;
	% 	  4 : "Time Server Option",
	% 	  5 : "Name Server Option",
decode_option_name(6) ->  dns_server;
	% 	  7 : "Log Server Option",
	% 	  8 : "Cookie Server Option",
	% 	  9 : "LPR Server Option",
	% 	 10 : "Impress Server Option",
	% 	 11 : "Resource Location Server Option",
decode_option_name(12) -> host_name;
	% 	 13 : "Boot File Size Option",
	% 	 14 : "Merit Dump File",
decode_option_name(15) -> domain_name;
	% 	 16 : "Swap Server",
	% 	 17 : "Root Path",
	% 	 18 : "Extensions Path",
	% 	 19 : "IP Forwarding Enable/Disable Option",
	% 	 20 : "Non-Local Source Routing Enable/Disable Option",
	% 	 21 : "Policy Filter Option",
	% 	 22 : "Maximum Datagram Reassembly Size",
	% 	 23 : "Default IP Time-to-live",
	% 	 24 : "Path MTU Aging Timeout Option",
	% 	 25 : "Path MTU Plateau Table Option",
	% 	 26 : "Interface MTU Option",
	% 	 27 : "All Subnets are Local Option",
decode_option_name(28) -> broadcast_address;
	% 	 29 : "Perform Mask Discovery Option",
	% 	 30 : "Mask Supplier Option",
	% 	 31 : "Perform Router Discovery Option",
	% 	 32 : "Router Solicitation Address Option",
	% 	 33 : "Static Route Option",
	% 	 34 : "Trailer Encapsulation Option",
	% 	 35 : "ARP Cache Timeout Option",
	% 	 36 : "Ethernet Encapsulation Option",
	% 	 37 : "TCP Default TTL Option",
	% 	 38 : "TCP Keepalive Interval Option",
	% 	 39 : "TCP Keepalive Garbage Option",
	% 	 40 : "Network Information Service Domain Option",
	% 	 41 : "Network Information Servers Option",
	% 	 42 : "Network Time Protocol Servers Option",
	% 	 43 : "Vendor Specific Information",
	% 	 44 : "NetBIOS over TCP/IP Name Server Option",
	% 	 45 : "NetBIOS over TCP/IP Datagram Distribution Server Option",
	% 	 46 : "NetBIOS over TCP/IP Node Type Option",
	% 	 47 : "NetBIOS over TCP/IP Scope Option",
	% 	 48 : "X Window System Font Server Option",
	% 	 49 : "X Window System Display Manager Option",
decode_option_name(50) -> requested_ip_address;
decode_option_name(51) -> lease_time;
decode_option_name(53) -> message_type;
decode_option_name(54) -> server_id;
decode_option_name(55) -> parameter_request;
	% 	 56 : "Message",
decode_option_name(57) -> max_message_size;
decode_option_name(58) -> renewal_time;
decode_option_name(59) -> rebinding_time;
	% 	 60 : "Vendor class identifier",
decode_option_name(61) -> client_id;
	% 	 64 : "Network Information Service+ Domain Option",
	% 	 65 : "Network Information Service+ Servers Option",
	% 	 66 : "TFTP server name",
	% 	 67 : "Bootfile name",
	% 	 68 : "Mobile IP Home Agent option",
	% 	 69 : "Simple Mail Transport Protocol (SMTP) Server Option",
	% 	 70 : "Post Office Protocol (POP3) Server Option",
	% 	 71 : "Network News Transport Protocol (NNTP) Server Option",
	% 	 72 : "Default World Wide Web (WWW) Server Option",
	% 	 73 : "Default Finger Server Option",
	% 	 74 : "Default Internet Relay Chat (IRC) Server Option",
	% 	 75 : "StreetTalk Server Option",
	% 	 76 : "StreetTalk Directory Assistance (STDA) Server Option",
decode_option_name(Op) ->
	io:format("Error decoding DHCP Option: ~p~n", [Op]),
	error.          %% I know, this should be an error..

%% decode a option number into a symbolic name
encode_option_name(subnet_mask)          -> 1;
encode_option_name(time_offset)          -> 2;
encode_option_name(router)               -> 3;
encode_option_name(dns_server)           -> 6;
encode_option_name(host_name)            -> 12;
encode_option_name(domain_name)          -> 15;
encode_option_name(broadcast_address)    -> 28;
encode_option_name(requested_ip_address) -> 50;
encode_option_name(lease_time)           -> 51;
encode_option_name(message_type)         -> 53;
encode_option_name(server_id)            -> 54;
encode_option_name(parameter_request)    -> 55;
encode_option_name(max_message_size)     -> 57;
encode_option_name(renewal_time)         -> 58;
encode_option_name(rebinding_time)       -> 59;
encode_option_name(client_id)            -> 61;
encode_option_name(Op) ->
  io:format("Error encoding DHCP Option: ~p~n", [Op]),
  115.              %% Seems to be unasigned..


%% Decode every known DHCP option
decode_option_value(subnet_mask, Value)          -> decode_value(ip_address, Value);
decode_option_value(router, Value)               -> decode_value(ip_address, Value);
decode_option_value(dns_server, Value)           -> decode_value(ip_address, Value);
decode_option_value(broadcast_address, Value)    -> decode_value(ip_address, Value);
decode_option_value(requested_ip_address, Value) -> decode_value(ip_address, Value);
decode_option_value(server_id, Value)            -> decode_value(ip_address, Value);
decode_option_value(time_offset, Value)          -> decode_value(quad_int, Value);
decode_option_value(lease_time, Value)           -> decode_value(quad_int, Value);
decode_option_value(renewal_time, Value)         -> decode_value(quad_int, Value);
decode_option_value(rebinding_time, Value)       -> decode_value(quad_int, Value);
decode_option_value(max_message_size, Value)     -> decode_value(word_int, Value);
decode_option_value(host_name, Value)            -> decode_value(string, Value);
decode_option_value(domain_name, Value)          -> decode_value(string, Value);
decode_option_value(message_type, Value)         -> decode_value(message_type, Value);
decode_option_value(_, Value)                    -> decode_value(binary, Value).

%% Decode all diferent kind of values in a DHCP option field 
decode_value(binary, Value)              -> Value;
decode_value(ip_address, Value)          -> ipv4_to_tuple(Value);
decode_value(string, Value)              -> binary_to_list(Value);
decode_value(quad_int, <<DWord:32/big>>) -> DWord;
decode_value(word_int, <<Word:16/big>>)  -> Word;
decode_value(message_type, <<1:8>>)      -> discover;
decode_value(message_type, <<2:8>>)      -> offer;
decode_value(message_type, <<3:8>>)      -> request;
decode_value(message_type, <<4:8>>)      -> decline;
decode_value(message_type, <<5:8>>)      -> ack;
decode_value(message_type, <<6:8>>)      -> nak;
decode_value(message_type, <<7:8>>)      -> release;
decode_value(message_type, <<8:8>>)      -> inform.

%% Encode every known DHCP option
encode_option_value(subnet_mask, Value)          -> encode_value(ip_address, Value);
encode_option_value(router, Value)               -> encode_value(ip_address, Value);
encode_option_value(dns_server, Value)           -> encode_value(ip_address, Value);
encode_option_value(broadcast_address, Value)    -> encode_value(ip_address, Value);
encode_option_value(requested_ip_address, Value) -> encode_value(ip_address, Value);
encode_option_value(server_id, Value)            -> encode_value(ip_address, Value);
encode_option_value(time_offset, Value)          -> encode_value(quad_int, Value);
encode_option_value(lease_time, Value)           -> encode_value(quad_int, Value);
encode_option_value(renewal_time, Value)         -> encode_value(quad_int, Value);
encode_option_value(rebinding_time, Value)       -> encode_value(quad_int, Value);
encode_option_value(max_message_size, Value)     -> encode_value(word_int, Value);
encode_option_value(host_name, Value)            -> encode_value(string, Value);
encode_option_value(domain_name, Value)          -> encode_value(string, Value);
encode_option_value(message_type, Value)         -> encode_value(message_type, Value);
encode_option_value(_, Value)                    -> encode_value(binary, Value).

%% Encode all diferent kind of values in a DHCP option field 
encode_value(binary, Value)                      -> Value;
encode_value(ip_address, Value)                  -> tuple_to_ipv4(Value);
encode_value(string, Value)                      -> list_to_binary(Value);
encode_value(quad_int, Value)                    -> <<Value:32/big>>;
encode_value(word_int, Value)                    -> <<Value:16/big>>;
encode_value(message_type, discover)             -> <<1:8>>;
encode_value(message_type, offer)                -> <<2:8>>;
encode_value(message_type, request)              -> <<3:8>>;
encode_value(message_type, decline)              -> <<4:8>>;
encode_value(message_type, ack)                  -> <<5:8>>;
encode_value(message_type, nak)                  -> <<6:8>>;
encode_value(message_type, release)              -> <<7:8>>;
encode_value(message_type, inform)               -> <<8:8>>;
encode_value(Name, Value)                        -> Value.






%% DHCPDISCOVER. 
%% Issued from the client to the server to solicit DHCP address assignment; the DHCPDISCOVER may include parameters 
%% or options required by the client.

%%{discover ,broadcast ,direct  ,{XID} ,{HWADDR},options}
%%{discover ,unicast   ,relayed ,{XID} ,{HWADDR},{GIADDR},{YIADDR},options}


%% DHCPOFFER. 
%% Issued from the server to the client indicating an IP address offer including its corresponding lease time (and 
%% other configuration parameters) to the client in response to a DHCPDISCOVER.

%%{offer}

%% DHCPREQUEST. 
%% Issued from the client to a server in response to a DHCPOF- FER to accept or reject the offered IP address, along 
%% with desired or additional parameter settings. The DHCPREQUEST is also used by clients desiring to extend or renew 
%% their existing IP address lease.

%% DHCPACK.
%% Issued from the server to the client to positively acknowledge the grant of the IP address lease and associated 
%% parameter settings. The client may now begin using the IP address and parameter values.

%% DHCPNAK.
%% Issued from the server to the client to negatively acknowledge the DHCP transaction. The client must cease the use 
%% of the IP address and reinitiate the process if necessary.

%% DHCPDECLINE.
%% Issued from the client to the server, to indicate that the IP address offered by the server is already in use by 
%% another client. The DHCP server will then typically mark the IP address as unavailable.

%% DHCPRELEASE.
%% Issued from the client to the server to inform the server that the client is relinquishing the IP address. The client 
%% must cease the use of the IP address thereafter.

%% DHCPINFORM.
%% Issued from the client to the server to request non-IP address configuration parameters from the server. The server 
%% will formulate a DHCPACK reply with the associated values as appropriate.

%% DHCPFORCERENEW.
%% Issued from the server to the client to force a client into the INIT state0 in order to obtain a new (different) 
%% IP address. Few clients have implemented support of this message.

%% DHCPLEASEQUERY.
%% Issued from a relay agent or other device to a server to determine if a given MAC address, IP address, or 
%% client-identifier value has an active lease and its associated lease parameter values according to the DHCP server 
%% (used primarily by broadband access concentrators or edge devices).

%% DHCPLEASEUNASSIGNED.
%% Issued from a server to a relay agent in response to a DHCPLEASEQUERY informing the relay agent that this server 
%% supports that address but there is no active lease.

%% DHCPLEASEUNKNOWN.
%% Issued from a server to a relay agent in response to a DHCPLEASEQUERY informing the relay agent that the server 
%% has no knowledge of the client specified in the query.
%% DHCPLEASEACTIVE. 
%% Issued from a server to a relay agent in response to a DHCPLEASEQUERY with the endpoint location and remaining 
%% lease time.

%%{offer}
%%{request}
%%{akcnowledge}
