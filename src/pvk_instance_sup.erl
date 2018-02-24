-module(pvk_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/1
        ,holder_srv/1
        ,devices_sup/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("plain_vulkan.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

-spec holder_srv(pid()) -> pid().
holder_srv(Sup) ->
    [{_, Srv, _, _}] = lists:filter(fun workers/1, supervisor:which_children(Sup)),
    Srv.

workers({_, _,'worker',_}) -> 'true';
workers({_, _,_, _}) -> 'false'.

-spec devices_sup(pid()) -> pid().
devices_sup(Sup) ->
    [{_, Srv, _, _}] = lists:filter(fun supers/1, supervisor:which_children(Sup)),
    Srv.

supers({_, _,'supervisor',_}) -> 'true';
supers({_, _,_, _}) -> 'false'.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(string()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(AppName) ->
    Name = list_to_atom(AppName),
    supervisor:start_link({local, Name}, ?MODULE, [AppName]).

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
-spec init(term()) -> {'ok', supervisor:child_spec()}.
init([AppName]) ->
    RestartStrategy = 'rest_for_one',
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [?WORKER(pvk_instance_holder, AppName)
               ,?SUPER(pvk_devices_sup)
               ],

    {'ok', {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
