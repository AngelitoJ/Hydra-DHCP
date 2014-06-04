%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel

-module(dhcp_options).

-compile(export_all).

-include("address_tools.hrl").



%% map DHCP options binary to a property list 
decode_options(Binary) -> decode_options(Binary, []).
decode_options(<<0:8, Rest/binary>>, []) -> decode_options(Rest, []);
decode_options(<<255:8, _/binary>>, Opts) -> Opts;                            % Final Marker. We are done
decode_options(<<Op:8, Len:8, Data:Len/binary, Rest/binary>>, Opts) ->        % Default
  Name  = decode_option_name(Op),
  Value = decode_option_value(Name, Data),
  decode_options(Rest,[{Name, Value }|Opts]).

%% map DHCP options property list into a binary
encode_options([{Name, Val} | Rest]) ->
  Op      = encode_option_name(Name),
  Data    = encode_option_value(Name, Val),
  Len     = size(Data),
  <<Op:8, Len:8, Data/binary, (encode_options(Rest))/binary>>;
encode_options([]) -> <<255:8>>.                                               % We are done!! (i guess..)


%% search for DHCP options and provide sane defaults.
option_search(requested_ip_address,Opts) -> 
  case lists:keysearch(requested_ip_address, 1, Opts) of
    {value, Value} -> Value;
    _              -> {requested_ip_address, {0, 0, 0, 0}}
    end;
option_search(Key,Opts) ->
  case lists:keysearch(Key, 1, Opts) of
    {value, Value} -> Value;
    _              -> not_found
    end.



%% decode a option number into a symbolic name, not all option are implemented..
decode_option_name(1) ->  subnet_mask;
decode_option_name(2) ->  time_offset;
decode_option_name(3) ->  router;
  %     4 : "Time Server Option",
  %     5 : "Name Server Option",
decode_option_name(6) ->  dns_server;
  %     7 : "Log Server Option",
  %     8 : "Cookie Server Option",
  %     9 : "LPR Server Option",
  %    10 : "Impress Server Option",
  %    11 : "Resource Location Server Option",
decode_option_name(12) -> host_name;
  %    13 : "Boot File Size Option",
  %    14 : "Merit Dump File",
decode_option_name(15) -> domain_name;
  %    16 : "Swap Server",
  %    17 : "Root Path",
  %    18 : "Extensions Path",
  %    19 : "IP Forwarding Enable/Disable Option",
  %    20 : "Non-Local Source Routing Enable/Disable Option",
  %    21 : "Policy Filter Option",
  %    22 : "Maximum Datagram Reassembly Size",
  %    23 : "Default IP Time-to-live",
  %    24 : "Path MTU Aging Timeout Option",
  %    25 : "Path MTU Plateau Table Option",
  %    26 : "Interface MTU Option",
  %    27 : "All Subnets are Local Option",
decode_option_name(28) -> broadcast_address;
  %    29 : "Perform Mask Discovery Option",
  %    30 : "Mask Supplier Option",
  %    31 : "Perform Router Discovery Option",
  %    32 : "Router Solicitation Address Option",
  %    33 : "Static Route Option",
  %    34 : "Trailer Encapsulation Option",
  %    35 : "ARP Cache Timeout Option",
  %    36 : "Ethernet Encapsulation Option",
  %    37 : "TCP Default TTL Option",
  %    38 : "TCP Keepalive Interval Option",
  %    39 : "TCP Keepalive Garbage Option",
  %    40 : "Network Information Service Domain Option",
  %    41 : "Network Information Servers Option",
  %    42 : "Network Time Protocol Servers Option",
  %    43 : "Vendor Specific Information",
  %    44 : "NetBIOS over TCP/IP Name Server Option",
  %    45 : "NetBIOS over TCP/IP Datagram Distribution Server Option",
  %    46 : "NetBIOS over TCP/IP Node Type Option",
  %    47 : "NetBIOS over TCP/IP Scope Option",
  %    48 : "X Window System Font Server Option",
  %    49 : "X Window System Display Manager Option",
decode_option_name(50) -> requested_ip_address;
decode_option_name(51) -> lease_time;
decode_option_name(53) -> message_type;
decode_option_name(54) -> server_id;
decode_option_name(55) -> parameter_request;
  %    56 : "Message",
decode_option_name(57) -> max_message_size;
decode_option_name(58) -> renewal_time;
decode_option_name(59) -> rebinding_time;
  %    60 : "Vendor class identifier",
decode_option_name(61) -> client_id;
  %    64 : "Network Information Service+ Domain Option",
  %    65 : "Network Information Service+ Servers Option",
  %    66 : "TFTP server name",
  %    67 : "Bootfile name",
  %    68 : "Mobile IP Home Agent option",
  %    69 : "Simple Mail Transport Protocol (SMTP) Server Option",
  %    70 : "Post Office Protocol (POP3) Server Option",
  %    71 : "Network News Transport Protocol (NNTP) Server Option",
  %    72 : "Default World Wide Web (WWW) Server Option",
  %    73 : "Default Finger Server Option",
  %    74 : "Default Internet Relay Chat (IRC) Server Option",
  %    75 : "StreetTalk Server Option",
  %    76 : "StreetTalk Directory Assistance (STDA) Server Option",
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
encode_value(_Name, Value)                        -> Value.

