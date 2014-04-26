%% Hydra DHCP Server project
%% (C) 2014 Angel J. Alvarez Miguel


-module(dhcp_server).
-author("angeljalvarezmiguel@gmail.com").
-compile(export_all).

%% Option descriptors and funs required for this module to work (see below for a explanation).

option_specs() ->
    Procesos = erlang:system_info(schedulers_online) * 2,
    {    
		%%List of getopt descriptors {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
        [
            {help,        $?,        "help",        undefined,                    "Show this help."},
            {debug,       $d,        "debug",       {integer, 0},                 "Show debug info."},
            {verbose,     $v,        "verbose",     undefined,                    "Show all actions performed."},
            {version,     $V,        "version",     undefined,                    "Show software version."},
            {procs,       $P,        "cores",       {integer, Procesos },         "Number of workers (default 2*core)."}
        ]
        %% args processing funs required for some options
        ,[
             fun check_help/1          % check for help requests at the command line.
            ,fun check_version/1       % check for software version.
            ,fun check_debug/1         % check for debug level among posible values.
        ]
    }.

% Entry point for escript(tm) execution path
% Setup main enviroment, do getopt parsing and custom args processing user messaging
% the setup OTP application

-spec main([string() | char()]) -> 'ok'.
main(Args) ->
   AppName = ?MODULE,        % this module needs to be named after the OTP real application.
   app_setup(AppName,Args).  % prepare a sane setup for app start-up


% application setup, we need to parse args from the escript invocation to makeup a sane options record to the app.
% We also need to scan how many module have opts processing, so we use getoptex(tm) for that.
% Finally we call app_main to start the OTP tree properly.

-spec app_setup(atom(),[string() | char()]) -> 'ok'.
app_setup(AppName,Args) ->

	%% Load application and retrieve some properties
    ok                       = application:load(AppName),
    {ok,Version}             = application:get_key(AppName,vsn),
    {ok,Description}         = application:get_key(AppName,description),
    AppFilename              = escript:script_name(),

    io:format("~s\nVersion: ~s\n\n",[Description,Version]),

	%% Scan app modules requiring options descriptor processing (Those exporting option_specs functions).
    OptionProviders          = module_tools:list_app_modules(AppName,exports,option_specs),

    %% Collect all option descriptors and funs into a single list.
    {OptSpecList,OptFunList} = getoptex:collect_option_providers(OptionProviders), 


    %% Parse args using option descriptors and then check args values using provided funs.
    case getoptex:parse_args(OptSpecList, Args, OptFunList) of

    {ok, {AppOpts, _OtherArgs}} ->                      %% Everything went Ok.
        app_main(AppName,AppOpts);     %% Start the application, AppOpts is a property list.

    {help} ->                                           %% Help request detected..
        getopt:usage(OptSpecList, AppFilename);         %% Provide info about usage.

    {version} ->                                        %% Software version requested..
        io:format("~s (v ~s)\n~s\n", 
        	[AppFilename, Version, Description]);       %% Show the info.

    {error, {invalid_option_arg, Data}} ->              %% Some argument was wrong..
        io:format("Error:\n\t Invalid option: ~p~n~n",  
        	[Data]),                                    %% Show the offending bits.
        getopt:usage(OptSpecList, AppFilename);         %% Provide info about usage.

    {error, {Reason, Data}} ->                          %% Something else went wrong.. 
        io:format("Error:\n\t~s ~p~n~n",
        	[Reason, Data]),                            %% Show some error diagnostics
        getopt:usage(OptSpecList, AppFilename)          %% Provide info about usage.

    end.

-spec app_main(atom(),[tuple()]) -> none().
app_main(AppName,Options) ->
   {A1,A2,A3} = now(),                                              %% get random seed from current time
   random:seed(A1, A2, A3),                                         %% feed the standard random number generator

   ok = application:set_env(AppName,cmdline_options,Options,5000),  %% Store options so app will get them on startup

   case application:start(AppName) of                               %% start the application

   ok ->                                                            %% ok, wait until we are done and then shutdown the app
      wait_for_app(60000),
      ok = application:stop(AppName);

   Other ->                                                         %% something went wrong..
      io:format("[MAIN] Application ~w terminated badly, with cause: ~p\n",[AppName,Other])

   end.


%% Wait for top supervisor to terminate or timeout 
wait_for_app() -> 
    wait_for_app(infinity).

wait_for_app(Timeout) ->
    erlang:monitor(process,dhcp_server_sup),
    receive
        {'DOWN', _, process, Pref, Reason} ->
            io:format("[MAIN] Application supervisor ~w terminated, with cause: ~p\n",[Pref,Reason]);
        stopped ->
            stopped
    after
        Timeout -> 
            io:format("[MAIN] Application Timeout\n",[]),
            stopped 
    end.



%% Args processing funs are a sort of monadic functions working on a Either OptionRecord Error..

%% Check for a help request and return a help tuple or let untouched the original property list.

-spec check_help([any()]) -> [any()] | {'help'}.
check_help(Opts) ->
    case proplists:get_bool(help,Opts) of               %% Did the user request help?
            true -> { help };                           %% Yes, forget everything else 
            false -> Opts                               %% No, we return the original list 
    end.

%% Check for a software version request and return a version tuple or let untouched the original property list.

-spec check_version([any()]) -> [any()] | {'version'}.
check_version(Opts) ->
    case proplists:get_bool(version,Opts) of
            true -> { version };
            false -> Opts
    end.

%% Check for a debug level request and add a suitable debug level tuple if needed or return untouched the original property list. 

-spec check_debug([any()]) -> [any()].
check_debug(Opts) ->
    Level = proplists:get_value(debug,Opts,0),               %% Get the debug level requested? (0 is the default). 
    case (Level < 0) or (Level > 3) of                       %% Is requested level between bounds?
        true ->  [{debug,0} | proplists:delete(debug,Opts)]; %% No, correct it and replace the wrong bits. 
        false -> Opts                                        %% Yes, return the original list.
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.

