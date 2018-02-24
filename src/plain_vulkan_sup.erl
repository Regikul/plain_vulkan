-module(plain_vulkan_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,new_instance/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("plain_vulkan.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new_instance(string()) -> supervisor:startchild_ret().
new_instance(AppName) ->
    SupName = list_to_atom(AppName),
    case whereis(SupName) of
        'undefined' -> supervisor:start_child(?SERVER, AppName);
        Pid -> {'error', {'already_started', Pid}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, supervisor:child_spec()}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [?SUPER(pvk_instance_sup)],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
