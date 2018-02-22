-module(plain_vulkan).

-on_load(init/0).

%% API exports
-export([
  hello_world/0,
  create_instance/1
]).

%%====================================================================
%% API functions
%%====================================================================

-spec hello_world() -> atom().
hello_world() -> erlang:nif_error({error, not_loaded}).

-spec create_instance(string()) -> {ok, term()} | error.
create_instance(_) -> erlang:nif_error({error, not_loaded}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() -> ok.
init() ->
  PrivDir = code:priv_dir(?MODULE),
  ok = erlang:load_nif(filename:join(PrivDir, "plain_vulkan_drv"), 0).