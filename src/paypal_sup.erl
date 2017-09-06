%%%-------------------------------------------------------------------
%% @doc paypal top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('paypal_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id, Module, Args), 
        {Id, {Module, start_link, Args}, permanent, 10, worker, [Module]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Plugins = application:get_env(paypal, plugins, []),
    Children = lists:map(fun start_plugin/1, Plugins),
    Request = ?CHILD(paypal_request, paypal_request, []),
    {ok, { {one_for_all, 0, 1}, [Request | Children]} }.

%%====================================================================
%% Internal functions
%%====================================================================
start_plugin(Plugin) ->
    PS = atom_to_list(Plugin),
    ModuleStr = "paypal_" ++ PS,
    Module = list_to_atom(ModuleStr),
    Config = application:get_env(paypal, Plugin, []),
    ?CHILD(Plugin, Module, [Config]).
