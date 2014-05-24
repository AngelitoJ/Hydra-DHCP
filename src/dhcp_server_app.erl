%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel



-module(dhcp_server_app).
-author("angeljalvarezmiguel@gmail.com").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->

   {ok, CmdlineOpts} = application:get_env(cmdline_options),    %% recover Opts from the enviroment
   AppOptions = [{start_args,StartArgs}|CmdlineOpts],           %% Combine start aargs with cmdline options

   case top_sup:start_link(AppOptions) of
   {ok, Pid} ->
       io:format("Application started...\n\n"),
      {ok, Pid};
   {error,Other} ->
      io:format("Application crashed! Error: ~p\n",[Other]),
      {error, Other}
   end.

stop(_State) ->
    io:format("Application stopped...\n\n"),
    ok.

