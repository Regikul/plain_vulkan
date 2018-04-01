-module(pvk_instance_holder).

-behaviour(gen_server).

-include("plain_vulkan.hrl").

%% API
-export([
  start_link/1,
  devices_list/1
]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {
  vk_instance_ref :: reference(),
  vk_devices = [] :: plain_vulkan:vk_physical_device()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec devices_list(pid()) -> [vk_physical_device_properties()].
devices_list(Srv) ->
  gen_server:call(Srv, {device_list}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(string()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(AppName) ->
    gen_server:start_link(?MODULE, [AppName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, state()} | {ok, state(), timeout() | hibernate} | {stop, term()} | ignore.
init([AppName]) ->
    {'ok', VkInstance} = plain_vulkan:create_instance(AppName),
    case plain_vulkan:enumerate_physical_devices(VkInstance) of
      {_, Devices} -> {ok, #state{vk_instance_ref = VkInstance
                                 ,vk_devices = Devices
                                 }};
      Error ->
        plain_vulkan:destroy_instance(VkInstance),
        {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call (term(), {pid(), Tag :: term()}, state()) ->
    {reply, term(), state()} |
    {reply, term(), state(), timeout() | hibernate} |
    {noreply, state()} |
    {noreply, state(), timeout() | hibernate} |
    {stop, term(), term(),state()} |
    {stop, term(), state()}.
handle_call({device_list}, _From, #state{vk_devices = Devices} = State) ->
    {reply, Devices, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), state()) ->
    {noreply, state()} |
    {noreply, state(), timeout() | hibernate} |
    {stop, term(), state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(timeout() | term(), state()) ->
    {noreply, state()} |
    {noreply, state(), timeout() | hibernate} |
    {stop, term(), state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(terminate_reason(), state()) -> term().
terminate(_Reason, #state{vk_instance_ref = VkInstance}) ->
    plain_vulkan:destroy_instance(VkInstance).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change  (term() | {down, term()}, state(), term()) -> {ok, state()} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
