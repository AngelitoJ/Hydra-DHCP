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
-include("address_tools.hrl").

-record(st, {
                             id        = undefined            %% This middleman server ID
                            ,server    = undefined            %% ServerId (server IP) for 'ignore' message purposes
                            ,fsm_cache = undefined            %% Local cache {HW_address, Pid} MAP of know client FSM's
                          }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
  Id       = proplists:get_value(who_you_are,Opts),
  gen_server:start_link({local, Id}, ?MODULE, Opts, []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
  Id       = proplists:get_value(who_you_are,Opts), 
  ServerId = proplists:get_value(server_id,Opts), 
  NewState = #st{ id = Id, server = ServerId, fsm_cache = dict:new() },

  io:format("[~p]: Init..\n", [Id]),
  
  {ok, NewState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% Handle a posible DHCP message from the udp driver
handle_cast({dhcp, _Scope, Packet}, #st{ id = Id, fsm_cache = Cache } = State) ->
  io:format("~p: DHCP msg received!!\n",[Id]),

  %% Here we go, decode packet
  {ok, Record}          = packet_to_record(Packet),          %% Decode Packet into a record
  ClientId              = get_client_id(Packet),             %% get the client id from the record contents
  Msg                   = record_to_msg(Record),             %% make up a suitable message to the FSM
  
  case cache:lookup_by_id(Cache, ClientId) of
    {ok, Pid} ->                                             %% We know the client fsm pid, proceed
                gen_fsm:send_event(Pid,Msg),
                {noreply, State};
    error     ->                                             %% We dont know the pid yet 
                case gen_server:call(dora_cache_srv, {get_pid, Id}) of
                  {ok, Pid}       ->                         %% Ok monitor the new pid, send the message and remember the pid
                                    erlang:monitor(process, Pid),
                                    gen_fsm:send_event(Pid,Msg),
                                    {noreply, #st{ fsm_cache = cache:insert(Cache,Id,Pid)} };
                  {error, Reason} ->                         %% Something went wrong, we have no pid, bail out
                                    {stop, Reason}
                 end

  end;

%% Handle a DORA fsm message
handle_cast({offer, Record}, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n",[Id, offer]),
    {noreply, State};

handle_cast({ack, Record}, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n",[Id, ack]),
    {noreply, State};

handle_cast({nack, Record}, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n",[Id, nack]),
    {noreply, State};

handle_cast({Other, Record}, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n",[Id, Other]),
    {noreply, State};


%% Handle a binary coded erlang term
handle_cast({udp, Scope, Packet}, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n",[Id, binary_to_term(Packet)]),
    {noreply, State};

%% Handle any unknow message
handle_cast(Msg, #st{id = Id} = State) ->
      io:format("[~p]: msg received!! ~p\n I must die!!\n",[Id, Msg]),
    {stop, i_dont_like_these_msgs, State}.

%% Handle a DOWN message from a diying fsm and remove it from the cache.
handle_info({'DOWN', _, process, Pid, Reason}, #st{ fsm_cache = Cache} = State) ->
    io:format("[~p]: Hey! client FSM ~p terminated with reason: ~p..\n", [?MODULE,Pid,Reason]),
    NewState = State#st{ fsm_cache = cache:remove_by_pid(Cache, Pid) },

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

record_to_msg(#dhcp_packet{ msg_type = MessageType } = Packet) ->
  MyPid = self(),             %% Send this process Pid so FSM dont have to deal with Pid resolution
  case MessageType of

    %% DHCPDISCOVER. Issued from the client to the server to solicit DHCP address assignment; the DHCPDISCOVER may 
    %% include parameters or options required by the client.
    discover -> {discover, MyPid, Packet};

    %% DHCPREQUEST. Issued from the client to a server in response to a DHCPOFFER to accept or reject the offered 
    %% IP address, along with desired or additional parameter settings. The DHCPREQUEST is also used by clients 
    %% desiring to extend or renew their existing IP address lease. We calculate the client state and inform the DORA
    %% machine about client state.
    request -> {request, client_is(Packet), MyPid, Packet}; %% {ignore, MyPid, Packet} if client accepted other server offer

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

%% make a DHCP record in response to a DORA machine tagged msg, different msgs map to a diferent options into the DHCP message.
% msg_to_record({Msg,Data}) ->
%   case Msg of

%     %% DHCPOFFER. Issued from the server to the client indicating an IP address offer including its corresponding 
%     %% lease time (and other configuration parameters) to the client in response to a DHCPDISCOVER.
%     offer -> {offer, Packet};

%     %% DHCPACK. Issued from the server to the client to positively acknowledge the grant of the IP address lease 
%     %% and associated parameter settings. The client may now begin using the IP address and parameter values.
%     ack -> {ack, Packet};

%     %% DHCPNAK. Issued from the server to the client to negatively acknowledge the DHCP transaction. The client 
%     %% must cease the use of the IP address and reinitiate the process if necessary.
%     nack -> {nack, Packet}
%   end.

%% get client id from DHCP record, use hwaddr as a default
get_client_id(#dhcp_packet{ chaddr = HWid, options = Opts }) -> 
    case dhcp_options:search(client_id,Opts) of
        {client_id, Clientid} ->
                                Clientid;
        not_found             ->
                                HWid
    end. 

%% figure client state 'init_reboot | selecting | renewing | rebinding ' from various fields 
client_is(#dhcp_packet{ options = Options, flags = Flags }) ->
    case dhcp_options:option_search(server_id, Options) of
    not_found             -> case dhcp_options:option_search(requested_address, Options) of
                             not_found              -> case (Flags bsr 15) == 1 of
                                                       false -> renewing;
                                                       _     -> rebinding
                                                       end;
                             {requested_address, _} -> init_reboot
                              end;
    {server_id, _}        -> selecting
    end.

%% map a DHCP binary packet into a record for easy processing.
packet_to_record(?DHCP_PACKET) ->
  DecodedOptions                      = dhcp_options:decode_options(Options),
  {message_type, MessageType}         = dhcp_options:option_search(message_type, DecodedOptions),
  {requested_ip_address, RequestedIp} = dhcp_options:option_search(requested_ip_address,DecodedOptions),


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
record_to_packet(#dhcp_packet{ op = Op, htype = HType, hlen = HLen, hops = Hops, xid = Xid, secs = Secs
            , flags = Flags, ciaddr = CIAddr, yiaddr = YIAddr, siaddr = SIAddr, giaddr = GIAddr
            , chaddr = CHAddr, sname = SName, file = File, options = DecodedOptions, msg_type = MessageType
            , rqaddr = RequestedIp, client_unicast = ClientScope } = Packet) ->

  EncodedOptions = dhcp_options:encode_options(DecodedOptions),
  EncodedPacket = <<Op:8, HType:8, HLen:8, Hops:8, Xid:32, Secs:16, Flags:16/big
            ,(tuple_to_ipv4(CIAddr))/binary, (tuple_to_ipv4(YIAddr))/binary
            ,(tuple_to_ipv4(SIAddr))/binary, (tuple_to_ipv4(GIAddr))/binary
            ,(tuple_to_mac(CHAddr))/binary, SName:512, File:1024
            ,<<99:8, 130:8, 83:8, 99:8>>/binary
            ,EncodedOptions/binary>>,
  {ok, EncodedPacket};

record_to_packet(_) ->
  {error, unknown_record}.




