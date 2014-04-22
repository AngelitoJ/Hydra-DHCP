%% Hydra DHCP Server project
%% Erlang GetOpt Extended services
%% @ 2012, 2014 Angel J. Alvarez
%% 2012 Initial design Catedromol project
%% 2014 Cosmetic Fixes
%%
%%
-module(getoptex).
-author("angeljalvarezmiguel@gmail.com").
-compile(export_all).


% Parse cmdline args with getopt and then checkout additional conditions on requested options
parse_args(SpecList, ArgList, FunList) ->
    case getopt:parse(SpecList, ArgList) of
    {ok, {Options, NonOptArgs}} ->                                                       %% Successfull getopt 
        COpts = lists:foldl(fun(Fun,Acc) when is_function(Fun,1), is_list(Acc) -> 
                                        Fun(Acc);
                                (_F,Acc) -> Acc end
                                ,Options                                                 %% Options returned from getopt
                                ,FunList                                                 %% List of funs collected
                            ),
        case is_list(COpts) of
        true ->
            {ok,{COpts,NonOptArgs}};                                                     %% make a ok result out of an Opts list
        false ->
            COpts                                                                        %% Oh boy! this is a error tuple...
        end;
    {error, {Reason, Data}} -> 
        {error, {Reason, Data}}                                                          %% getopt returned some error
    end.


%% Fold over a module list accumulating optspecs and funs exported
collect_option_providers(ModuleList) ->
    collect_option_providers([],[],ModuleList). 

collect_option_providers(Opts,Funs,[]) ->
        {lists:reverse(Opts),lists:reverse(Funs)};

collect_option_providers(Opts,Funs,[Module|RestList]) ->
    {NewOpts,NewFuns} = Module:option_specs(),
    collect_option_providers(NewOpts ++ Opts,NewFuns ++ Funs,RestList).
