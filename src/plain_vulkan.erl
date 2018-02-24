-module(plain_vulkan).

-on_load(init/0).

%% API exports
-export([
  create_instance/1,
  destroy_instance/1,
  enum_phy_devs/1,
  load_phy_devs/2
]).

%%====================================================================
%% API functions
%%====================================================================

-spec create_instance(string()) -> {ok, term()} | error.
create_instance(_) -> erlang:nif_error({error, not_loaded}).

-spec destroy_instance(any()) -> ok.
destroy_instance(_) -> erlang:nif_error({error, not_loaded}).

-spec enum_phy_devs(any()) -> any().
enum_phy_devs(_) -> erlang:nif_error({error, not_loaded}).

-spec load_phy_devs(any(), any()) -> any().
load_phy_devs(_Instance, _Count) -> erlang:nif_error({error, not_loaded}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() -> ok.
init() ->
  PrivDir = code:priv_dir(?MODULE),
  ok = erlang:load_nif(filename:join(PrivDir, "plain_vulkan_drv"), 0).
