%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(middleman_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% DHCP related functions
-include("dhcp_messages.hrl").

-record(middleman_state, {
                             id        = undefined            %% This middleman server ID
                            ,fsm_cache = undefined            %% Local cache {HW_address, Pid} MAP of know client FSM's
                          }).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
	Id = proplists:get_value(who_you_are,Opts), 
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Id = proplists:get_value(who_you_are,Args), 
	io:format("~p: Init with Args: ~w\n", [Id,Args]),
    NewState = #middleman_state{ id = Id, fsm_cache = dict:new() },
    {ok, NewState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% Handle a posible DHCP message from the udp driver
handle_cast({dhcp, _Scope, Packet}, #middleman_state{ id = Id, fsm_cache = Cache } = State) ->
  io:format("~p: DHCP msg received!!\n",[Id]),

  %% Here we go, decode packet
  {ok, Record}          = packet_to_record(Packet),          %% Decode Packet into a record
  ClientId              = get_client_id(Packet),             %% get the client id from the record contents
  {ClientFSM, NewCache} = cache_get_pid(Cache,ClientId),     %% Lookup for the client FSM (and spawn a new one if needed) 
  Msg                   = record_to_msg(Record),             %% make up a suitable message to the FSM

  gen_fsm:send_event(Pid,Msg),

      
  {noreply, State#middleman_state{ fsm_cache = NewCache }};

%% Handle a binary coded erlang term
handle_cast({udp, Scope, Packet}, #middleman_state{id = Id} = State) ->
      io:format("~p: msg received!! ~p\n",[Id, binary_to_term(Packet)]),
    {noreply, State};

%% Handle any unknow message
handle_cast(Msg, #middleman_state{id = Id} = State) ->
      io:format("~p: msg received!! ~p\n I must die!!\n",[Id, Msg]),
    {stop, i_dont_like_these_msgs, State}.

%% Handle a DOWN message from a diying fsm and remove it from the cache.
handle_info({'DOWN', _, process, Pid, Reason}, #middelman_state{ fsm_cache = Cache} = State) ->
    io:format("~p: Hey! client FSM ~p terminated with reason: ~p..\n", [?MODULE,Pid,Reason]),
    NewState = State#middleman_state{ fsm_cache = cache_remove_pid(Cache, Pid) },

    {noreply,  NewState };

handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% make a tagged msg suitable to be sent from/to a DORA gen_fsm. Tags allow easier processing in the fsm, because
%% most combinations of field in the DHCP packet will map to a diferent message.
record_to_msg(?DHCP_RECORD) ->
  myPid = self(),             %% Send this process Pid so FSM dont have to deal with Pid resolution
  case MessageType of

    %% DHCPDISCOVER. Issued from the client to the server to solicit DHCP address assignment; the DHCPDISCOVER may 
    %% include parameters or options required by the client.
    discover -> {discover, MyPid, Packet};

    %% DHCPREQUEST. Issued from the client to a server in response to a DHCPOFFER to accept or reject the offered 
    %% IP address, along with desired or additional parameter settings. The DHCPREQUEST is also used by clients 
    %% desiring to extend or renew their existing IP address lease.
    request -> {request, MyPid, Packet}; %% {ignore, MyPid, Packet} if client accepted other server offer

    %% DHCPDECLINE. Issued from the client to the server, to indicate that the IP address offered by the server is 
    %% already in use by another client. The DHCP server will then typically mark the IP address as unavailable.
    decline -> {decline, MyPid, Packet};

    %% DHCPRELEASE. Issued from the client to the server to inform the server that the client is relinquishing 
    %% the IP address. The client must cease the use of the IP address thereafter.
    release -> {release, MyPid, Packet};

    %% DHCPINFORM. Issued from the client to the server to request non-IP address configuration parameters from 
    %% the server. The server will formulate a DHCPACK reply with the associated values as appropriate.
    inform -> {inform, MyPid, Packet};

    %% DHCPFORCERENEW. Issued from the server to the client to force a client into the INIT state0 in order to 
    %% obtain a new (different) IP address. Few clients have implemented support of this message.
    %% DHCPLEASEQUERY. Issued from a relay agent or other device to a server to determine if a given MAC address, 
    %% IP address, or client-identifier value has an active lease and its associated lease parameter values according 
    %% to the DHCP server (used primarily by broadband access concentrators or edge devices).
    %% DHCPLEASEUNASSIGNED. Issued from a server to a relay agent in response to a DHCPLEASEQUERY informing the relay 
    %% agent that this server supports that address but there is no active lease.
    %% DHCPLEASEUNKNOWN. Issued from a server to a relay agent in response to a DHCPLEASEQUERY informing the relay 
    %% agent that the server has no knowledge of the client specified in the query.
    %% DHCPLEASEACTIVE. Issued from a server to a relay agent in response to a DHCPLEASEQUERY with the endpoint 
    %% location and remaining lease time.
    Other -> {Other, MyPid, Packet}
  end.

%% make a DHCP record in responde to a DORA machine tagged msg, different msgs map to a diferent options into the DHCP message.
msg_to_record({Msg,Data}) ->
  case Msg of

    %% DHCPOFFER. Issued from the server to the client indicating an IP address offer including its corresponding 
    %% lease time (and other configuration parameters) to the client in response to a DHCPDISCOVER.
    offer -> {offer, Packet};

    %% DHCPACK. Issued from the server to the client to positively acknowledge the grant of the IP address lease 
    %% and associated parameter settings. The client may now begin using the IP address and parameter values.
    ack -> {ack, Packet};

    %% DHCPNAK. Issued from the server to the client to negatively acknowledge the DHCP transaction. The client 
    %% must cease the use of the IP address and reinitiate the process if necessary.
    nack -> {nack, Packet}
  end.

%% get client id from DHCP record, use hwaddr as a default
get_client_id(#dhcp_packet{ chaddr = Id }) -> Id. 


%% map a DHCP binary packet into a record for easy processing.
packet_to_record(?DHCP_PACKET) ->
  DecodedOptions                      = dhcp_decode_options(Options),
  {message_type, MessageType}         = dhcp_option_search(message_type, DecodedOptions),
  {requested_ip_address, RequestedIp} = dhcp_option_search(requested_ip_address,DecodedOptions),


  Record = #dhcp_packet{
           msg_type       = MessageType
          ,op             = Op
          ,htype          = HType                   %% Should we inspect this?
          ,hlen           = HLen                    %% Should we care of this?
          ,hops           = Hops
          ,xid            = Xid
          ,secs           = Secs
          ,flags          = Flags
          ,ciaddr         = ipv4_to_tuple(CIAddr)
          ,yiaddr         = ipv4_to_tuple(YIAddr)
          ,siaddr         = ipv4_to_tuple(SIAddr)
          ,giaddr         = ipv4_to_tuple(GIAddr)
          ,chaddr         = mac_to_tuple(CHAddr)     %% Right now we asume is plain ethernet 6 bytes
          ,rqaddr         = RequestedIp
          ,sname          = SName
          ,file           = File
          ,options        = DecodedOptions
          ,client_unicast = Flags band 16#8000       
                },
  {ok, Record};

packet_to_record(Unknown) ->
  {error, unknown_packet}.

%% Generic mapping of DHCP binary packet to record, (TODO: oveload this to build specific responses)
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
dhcp_decode_options(<<255:8, _/binary>>, Opts) -> Opts;                            % Final Marker. We are done
dhcp_decode_options(<<Op:8, Len:8, Data:Len/binary, Rest/binary>>, Opts) ->        % Default
  Name  = dhcp_decode_option_name(Op),
  Value = dhcp_decode_option_value(Name, Data),
  dhcp_decode_options(Rest,[{Name, Value }|Opts]).

%% map DHCP options property list into a binary
dhcp_encode_options([{Name, Val} | Rest]) ->
  Op      = dhcp_encode_option_name(Name),
  Data    = dhcp_encode_option_value(Name, Val),
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
dhcp_decode_option_name(1) ->  subnet_mask;
dhcp_decode_option_name(2) ->  time_offset;
dhcp_decode_option_name(3) ->  router;
  %     4 : "Time Server Option",
  %     5 : "Name Server Option",
dhcp_decode_option_name(6) ->  dns_server;
  %     7 : "Log Server Option",
  %     8 : "Cookie Server Option",
  %     9 : "LPR Server Option",
  %    10 : "Impress Server Option",
  %    11 : "Resource Location Server Option",
dhcp_decode_option_name(12) -> host_name;
  %    13 : "Boot File Size Option",
  %    14 : "Merit Dump File",
dhcp_decode_option_name(15) -> domain_name;
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
dhcp_decode_option_name(28) -> broadcast_address;
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
dhcp_decode_option_name(50) -> requested_ip_address;
dhcp_decode_option_name(51) -> lease_time;
dhcp_decode_option_name(53) -> message_type;
dhcp_decode_option_name(54) -> server_id;
dhcp_decode_option_name(55) -> parameter_request;
  %    56 : "Message",
dhcp_decode_option_name(57) -> max_message_size;
dhcp_decode_option_name(58) -> renewal_time;
dhcp_decode_option_name(59) -> rebinding_time;
  %    60 : "Vendor class identifier",
dhcp_decode_option_name(61) -> client_id;
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
dhcp_decode_option_name(Op) ->
  io:format("Error decoding DHCP Option: ~p~n", [Op]),
  error.          %% I know, this should be an error..

%% decode a option number into a symbolic name
dhcp_encode_option_name(subnet_mask)          -> 1;
dhcp_encode_option_name(time_offset)          -> 2;
dhcp_encode_option_name(router)               -> 3;
dhcp_encode_option_name(dns_server)           -> 6;
dhcp_encode_option_name(host_name)            -> 12;
dhcp_encode_option_name(domain_name)          -> 15;
dhcp_encode_option_name(broadcast_address)    -> 28;
dhcp_encode_option_name(requested_ip_address) -> 50;
dhcp_encode_option_name(lease_time)           -> 51;
dhcp_encode_option_name(message_type)         -> 53;
dhcp_encode_option_name(server_id)            -> 54;
dhcp_encode_option_name(parameter_request)    -> 55;
dhcp_encode_option_name(max_message_size)     -> 57;
dhcp_encode_option_name(renewal_time)         -> 58;
dhcp_encode_option_name(rebinding_time)       -> 59;
dhcp_encode_option_name(client_id)            -> 61;
dhcp_encode_option_name(Op) ->
  io:format("Error encoding DHCP Option: ~p~n", [Op]),
  115.              %% Seems to be unasigned..


%% Decode every known DHCP option
dhcp_decode_option_value(subnet_mask, Value)          -> decode_value(ip_address, Value);
dhcp_decode_option_value(router, Value)               -> decode_value(ip_address, Value);
dhcp_decode_option_value(dns_server, Value)           -> decode_value(ip_address, Value);
dhcp_decode_option_value(broadcast_address, Value)    -> decode_value(ip_address, Value);
dhcp_decode_option_value(requested_ip_address, Value) -> decode_value(ip_address, Value);
dhcp_decode_option_value(server_id, Value)            -> decode_value(ip_address, Value);
dhcp_decode_option_value(time_offset, Value)          -> decode_value(quad_int, Value);
dhcp_decode_option_value(lease_time, Value)           -> decode_value(quad_int, Value);
dhcp_decode_option_value(renewal_time, Value)         -> decode_value(quad_int, Value);
dhcp_decode_option_value(rebinding_time, Value)       -> decode_value(quad_int, Value);
dhcp_decode_option_value(max_message_size, Value)     -> decode_value(word_int, Value);
dhcp_decode_option_value(host_name, Value)            -> decode_value(string, Value);
dhcp_decode_option_value(domain_name, Value)          -> decode_value(string, Value);
dhcp_decode_option_value(message_type, Value)         -> decode_value(message_type, Value);
dhcp_decode_option_value(_, Value)                    -> decode_value(binary, Value).

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
dhcp_encode_option_value(subnet_mask, Value)          -> encode_value(ip_address, Value);
dhcp_encode_option_value(router, Value)               -> encode_value(ip_address, Value);
dhcp_encode_option_value(dns_server, Value)           -> encode_value(ip_address, Value);
dhcp_encode_option_value(broadcast_address, Value)    -> encode_value(ip_address, Value);
dhcp_encode_option_value(requested_ip_address, Value) -> encode_value(ip_address, Value);
dhcp_encode_option_value(server_id, Value)            -> encode_value(ip_address, Value);
dhcp_encode_option_value(time_offset, Value)          -> encode_value(quad_int, Value);
dhcp_encode_option_value(lease_time, Value)           -> encode_value(quad_int, Value);
dhcp_encode_option_value(renewal_time, Value)         -> encode_value(quad_int, Value);
dhcp_encode_option_value(rebinding_time, Value)       -> encode_value(quad_int, Value);
dhcp_encode_option_value(max_message_size, Value)     -> encode_value(word_int, Value);
dhcp_encode_option_value(host_name, Value)            -> encode_value(string, Value);
dhcp_encode_option_value(domain_name, Value)          -> encode_value(string, Value);
dhcp_encode_option_value(message_type, Value)         -> encode_value(message_type, Value);
dhcp_encode_option_value(_, Value)                    -> encode_value(binary, Value).

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

