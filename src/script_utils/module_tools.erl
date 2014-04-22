%% Hydra DHCP Server project
%% Module utils functions
%% @ 2012 Angel Alvarez  Initial design
%% (C) 2014 Angel J. Alvarez Miguel

-module(module_tools).
-author("angel@uah.es").
-compile(export_all).

% get modules composing a given application
-spec list_app_modules(atom(),atom(),atom()) -> [atom() | tuple()].
list_app_modules(App,Class,Key) when is_atom(App), is_atom(Class), is_atom(Key) ->
    {ok,ModuleList}  = application:get_key(App,modules),
    list_modules(Class,Key,ModuleList).

% get modules exporting a value
-spec list_modules(atom(),atom(),[atom() | tuple()]) -> [atom() | tuple()].
list_modules(Class,Key,AppModules) when is_atom(Class),is_atom(Key) ->
    % Qué modulos exportan una valor de la clase pedida?
    Modules = [ X || X <- AppModules, lists:keymember(Key,1,X:module_info(Class)) ],
    Modules.