-module(plain_vulkan).

-on_load(init/0).

%% API exports
-export([
  create_instance/1,
  destroy_instance/1,
  count_physical_devices/1,
  enumerate_physical_devices/2
]).

%%====================================================================
%% API functions
%%====================================================================

-spec create_instance(string()) -> {ok, term()} | error.
create_instance(_) -> erlang:nif_error({error, not_loaded}).

-spec destroy_instance(any()) -> ok.
destroy_instance(_) -> erlang:nif_error({error, not_loaded}).

-spec count_physical_devices(any()) -> any().
count_physical_devices(_) -> erlang:nif_error({error, not_loaded}).

-spec enumerate_physical_devices(any(), any()) -> any().
enumerate_physical_devices(_Instance, _Count) -> erlang:nif_error({error, not_loaded}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() -> ok.
init() ->
  PrivDir = code:priv_dir(?MODULE),
  ok = erlang:load_nif(filename:join(PrivDir, "plain_vulkan_drv"), 0).
