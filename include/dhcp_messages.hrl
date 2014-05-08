%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


%% Record to hold decoded values from a DHCP UPD packet
-record (dhcp_packet, {
						 msg_type                        %% Message type (decoded from options)
						,op             = 0                     %% Operation code
						,htype          = 0                     %% Hardware address type
						,hlen           = 0                     %% Hardware address length
						,hops           = 0                     %% Number of Hops (incremented by routers)
						,xid            = 0                     %% Transaction ID
						,secs           = 0                     %% Second elapsed
						,flags          = 0                     %% Flags (Broadcast bit, everything else is reserved)
						,ciaddr         = {0, 0, 0, 0}          %% Client IP address
						,yiaddr         = {0, 0, 0, 0}          %% Offered IP address
						,siaddr         = {0, 0, 0, 0}          %% Server IP adress
						,giaddr         = {0, 0, 0, 0}          %% Relay IP Address
						,chaddr         = {0, 0, 0, 0, 0, 0}    %% Client HW address (MAC address)
						,rqaddr         = {0, 0, 0, 0}          %% Requested IP address (decoded from Option 50)
						,sname          = 0                     %% DHCP Server name
						,file           = 0                     %% Boot file name, null or absolute path
						,options        = []                    %% DHCP options property list
						,client_unicast = true                  %% Does client support unicast? (from client flags) 
					   }).


%% Macros for fast pattern matching
-define(DHCP_PACKET, <<Op:8, HType:8, HLen:8, Hops:8, Xid:32, Secs:16, Flags:16/big
						, CIAddr:4/binary, YIAddr:4/binary, SIAddr:4/binary, GIAddr:4/binary, CHAddr:16/binary
						, SName:512, File:1024, 99:8, 130:8, 83:8, 99:8, Options/binary>>).

-define(DHCP_RECORD, "#dhcp_packet{ op = Op, htype = HType, hlen = HLen, hops = Hops, xid = Xid, secs = Secs
						, flags = Flags, ciaddr = CIAddr, yiaddr = YIAddr, siaddr = SIAddr, giaddr = GIAddr
						, chaddr = CHAddr, sname = SName, file = File, options = DecodedOptions, msg_type = MessageType
						, rqaddr = RequestedIp, client_unicast = ClientScope } = Packet").








