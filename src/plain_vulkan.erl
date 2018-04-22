-module(plain_vulkan).

-on_load(init/0).

%% API exports
-export([
  create_instance/1,
  destroy_instance/1,
  count_instance_layer_properties/0,
  count_instance_extension_properties/1,
  enumerate_instance_extension_properties/2,
  enumerate_instance_extension_properties/1,
  count_physical_devices/1,
  enumerate_physical_devices/2,
  enumerate_physical_devices/1,
  get_physical_device_properties/1,
  get_physical_device_features/1,
  get_physical_device_queue_family_count/1,
  get_physical_device_queue_family_properties/2,
  get_physical_device_queue_family_properties/1,
  create_device/2,
  destroy_device/1,
  get_device_queue/3,
  create_command_pool/2
]).

-type vk_instance() :: reference().
-type vk_device() :: reference().
-type vk_queue() :: reference().
-type vk_command_pool() :: reference().
-type vk_physical_device() :: reference().
-type vk_physical_devices() :: [vk_physical_device()].
-type vk_enumerate_dev_ret() :: {ok, vk_physical_devices()}
                                | {incomplete, vk_physical_devices()}
                                | out_of_device_memory
                                | out_of_host_memory
                                | init_failed.
-type vk_count_dev_ret() :: {ok, non_neg_integer()}
                            | out_of_device_memory
                            | out_of_host_memory
                            | init_failed.

-export_type([
  vk_device/0,
  vk_instance/0,
  vk_queue/0,
  vk_command_pool/0,
  vk_physical_device/0,
  vk_physical_devices/0,
  vk_enumerate_dev_ret/0,
  vk_count_dev_ret/0
]).

-include("plain_vulkan.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec create_instance(string()) -> either(vk_instance(), atom()).
create_instance(_Name) -> erlang:nif_error({error, not_loaded}).

-spec destroy_instance(vk_instance()) -> ok.
destroy_instance(_Instance) -> erlang:nif_error({error, not_loaded}).

-spec count_instance_layer_properties() -> non_neg_integer().
count_instance_layer_properties() -> erlang:nif_error({error, not_loaded}).

-spec count_instance_extension_properties(vk_instance()) -> non_neg_integer().
count_instance_extension_properties(_Instance) -> erlang:nif_error({error, not_loaded}).

-spec enumerate_instance_extension_properties(vk_instance(), pos_integer()) -> term().
enumerate_instance_extension_properties(_Instace, _Count) -> erlang:nif_error({error, not_loaded}).

-spec enumerate_instance_extension_properties(vk_instance()) -> term().
enumerate_instance_extension_properties(Instance) ->
  case count_instance_extension_properties(Instance) of
    {'ok', Count} -> case enumerate_instance_extension_properties(Instance, Count) of
                       {ok, _} = Ret -> Ret;
                       _Else -> _Else
                     end;
    _Else -> _Else
  end.

-spec count_physical_devices(vk_instance()) -> vk_count_dev_ret().
count_physical_devices(_Instance) -> erlang:nif_error({error, not_loaded}).

-spec enumerate_physical_devices(vk_instance(), non_neg_integer()) -> vk_enumerate_dev_ret().
enumerate_physical_devices(_Instance, _Count) -> erlang:nif_error({error, not_loaded}).

-spec enumerate_physical_devices(vk_instance()) -> vk_enumerate_dev_ret().
enumerate_physical_devices(Instance) ->
  case count_physical_devices(Instance) of
    {ok, Count} -> case enumerate_physical_devices(Instance, Count) of
                     {ok, _} = Ret -> Ret;
                     _Else -> _Else
                   end;
    _Else -> _Else
  end.

-spec get_physical_device_properties(vk_physical_device()) -> vk_physical_device_properties().
get_physical_device_properties(_Device) -> erlang:nif_error({error, not_loaded}).

-spec get_physical_device_features(vk_physical_device()) -> vk_physical_device_features().
get_physical_device_features(_Device) -> erlang:nif_error({error, not_loaded}).

-spec get_physical_device_queue_family_count(vk_physical_device()) -> non_neg_integer().
get_physical_device_queue_family_count(_Device) -> erlang:nif_error({error, not_loaded}).

-spec get_physical_device_queue_family_properties_nif(vk_physical_device(), pos_integer()) -> {ok, [vk_queue_family_properties()]}.
get_physical_device_queue_family_properties_nif(_Device, _Count) -> erlang:nif_error({error, not_loaded}).

-spec get_physical_device_queue_family_properties(vk_physical_device(), pos_integer()) -> {ok, [vk_queue_family_properties()]}.
get_physical_device_queue_family_properties(Device, Count) ->
  case get_physical_device_queue_family_properties_nif(Device, Count) of
    {ok, Props} -> lists:map(fun queue_family_props_bits_to_list/1, Props);
    Else -> Else
  end.

-spec queue_family_props_bits_to_list(vk_queue_family_properties()) -> vk_queue_family_properties().
queue_family_props_bits_to_list(#vk_queue_family_properties{queueFlags = IntFlags} = Props) ->
  Graphics = if_bit(IntFlags, 16#1, 'graphics'),
  Compute = if_bit(IntFlags, 16#2, 'compute'),
  Transfer = if_bit(IntFlags, 16#4, 'transfer'),
  Sparse = if_bit(IntFlags, 16#8, 'sparse_binding'),
  Props#vk_queue_family_properties{queueFlags = lists:flatten([Graphics, Compute, Transfer, Sparse])}.

if_bit(Val, Bit, Success) ->
  case Val band Bit of
    Bit -> [Success];
    _Else -> []
  end.

-spec get_physical_device_queue_family_properties(vk_physical_device()) -> {ok, [vk_queue_family_properties()]}.
get_physical_device_queue_family_properties(Device) ->
  case get_physical_device_queue_family_count(Device) of
    {ok, Count} -> case get_physical_device_queue_family_properties(Device, Count) of
                     {ok, _} = Ret -> Ret;
                     _Else -> _Else
                   end;
    _Else -> _Else
  end.

-spec create_device(vk_physical_device(), vk_device_create_info()) -> either(vk_device(), atom()).
create_device(_PhysDev, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

-spec destroy_device(term()) -> 'ok'.
destroy_device(_LogicDev) -> erlang:nif_error({error, not_loaded}).

-spec get_device_queue(vk_device(), pos_integer(), pos_integer()) -> {ok, vk_queue()}.
get_device_queue(_Dev, _QueueFamilyIndex, _QueueIndex) -> erlang:nif_error({error, not_loaded}).

-spec create_command_pool(vk_device(), vk_command_pool_create_info()) -> either(vk_command_pool(), atom()).
create_command_pool(_Dev, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() -> ok.
init() ->
  PrivDir = code:priv_dir(?MODULE),
  ok = erlang:load_nif(filename:join(PrivDir, "plain_vulkan_drv"), 0).
