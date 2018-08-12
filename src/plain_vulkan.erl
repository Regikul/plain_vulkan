-module(plain_vulkan).

-on_load(init/0).

%% API exports
-export([
  create_instance/1, destroy_instance/1,
  count_instance_layer_properties/0,
  count_instance_extension_properties/1,
  enumerate_instance_extension_properties/1,
  count_physical_devices/1,
  enumerate_physical_devices/1,
  get_physical_device_properties/1,
  get_physical_device_features/1,
  get_physical_device_memory_properties/1,
  get_physical_device_queue_family_count/1,
  get_physical_device_queue_family_properties/1,
  get_device_queue/3,
  create_device/2, destroy_device/1, device_wait_idle/1,
  create_command_pool/2, destroy_command_pool/2,
  create_buffer/2, destroy_buffer/2, get_buffer_memory_requirements/2,
  allocate_memory/2, free_memory/2, bind_buffer_memory/4,
  create_descriptor_set_layout/2, destroy_descriptor_set_layout/2,
  create_descriptor_pool/2, destroy_descriptor_pool/2
]).

-type vk_instance() :: reference().
-type vk_device() :: reference().
-type vk_queue() :: reference().
-type vk_buffer() :: reference().
-type vk_command_pool() :: reference().
-type vk_device_memory() :: reference().
-type vk_physical_device() :: reference().
-type vk_descriptor_set_layout() :: reference().
-type vk_descriptor_pool() :: reference().
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
  vk_buffer/0,
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

-spec get_physical_device_memory_properties_nif(vk_physical_device()) -> vk_physical_device_memory_properties().
get_physical_device_memory_properties_nif(_Dev) -> erlang:nif_error({error, not_loaded}).

-spec memory_types_flags() -> proplists:proplist().
memory_types_flags() ->
  [{device_local, 16#01}
   ,{host_visible, 16#02}
   ,{host_coherent, 16#04}
   ,{host_cached, 16#08}
   ,{lazily_allocated, 16#10}
   ,{protected, 16#20}
  ].

-spec memory_heaps_flags() -> proplists:proplist().
memory_heaps_flags() ->
  [{device_local, 16#1}
   ,{multi_instance, 16#2}
  ].

-spec get_physical_device_memory_properties(vk_physical_device()) -> vk_physical_device_memory_properties().
get_physical_device_memory_properties(Device) ->
  #vk_physical_device_memory_properties{memory_types = MemoryTypes
                                        ,memory_heaps = MemoryHeaps
                                       } = get_physical_device_memory_properties_nif(Device),
  MemTypes = lists:map(fun memory_type_bits_to_list/1, MemoryTypes),
  MemHeaps = lists:map(fun memory_heaps_bits_to_list/1, MemoryHeaps),
  #vk_physical_device_memory_properties{memory_heaps = MemHeaps, memory_types = MemTypes}.

-spec memory_type_bits_to_list(vk_memory_type()) -> vk_memory_type().
memory_type_bits_to_list({Bits, Index}) -> {plain_vulkan_util:unfold_flags(Bits, memory_types_flags()), Index}.

-spec memory_heaps_bits_to_list(vk_memory_type()) -> vk_memory_type().
memory_heaps_bits_to_list({Size, Bits}) -> {Size, plain_vulkan_util:unfold_flags(Bits, memory_heaps_flags())}.

-spec get_physical_device_queue_family_properties(vk_physical_device(), pos_integer()) -> {ok, [vk_queue_family_properties()]}.
get_physical_device_queue_family_properties(Device, Count) ->
  case get_physical_device_queue_family_properties_nif(Device, Count) of
    {ok, Props} -> {ok, lists:map(fun queue_family_props_bits_to_list/1, Props)};
    Else -> Else
  end.

-spec queue_family_properties_flags() -> proplists:proplist().
queue_family_properties_flags() ->
  [{graphics, 16#1}
   ,{compute, 16#2}
   ,{transfer, 16#4}
   ,{sparse_binding, 16#8}
  ].

-spec queue_family_props_bits_to_list(vk_queue_family_properties()) -> vk_queue_family_properties().
queue_family_props_bits_to_list(#vk_queue_family_properties{queueFlags = IntFlags} = Props) ->
  ListFlags = plain_vulkan_util:unfold_flags(IntFlags, queue_family_properties_flags()),
  Props#vk_queue_family_properties{queueFlags = ListFlags}.

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

-spec device_wait_idle(vk_device()) -> 'ok'.
device_wait_idle(_LogicDev) -> erlang:nif_error({error, not_loaded}).

-spec destroy_device(vk_device()) -> 'ok'.
destroy_device(_LogicDev) -> erlang:nif_error({error, not_loaded}).

-spec get_device_queue(vk_device(), pos_integer(), pos_integer()) -> vk_queue().
get_device_queue(_Dev, _QueueFamilyIndex, _QueueIndex) -> erlang:nif_error({error, not_loaded}).

-spec command_pool_flags() -> proplists:proplist().
command_pool_flags() ->
  [{transient, 2#01}
   ,{reset, 2#10}
  ].

-spec create_command_pool(vk_device(), vk_command_pool_create_info()) -> either(vk_command_pool(), atom()).
create_command_pool(Dev, #vk_command_pool_create_info{flags = ListFlags} = CreateInfo) ->
  IntFlags = plain_vulkan_util:fold_flags(ListFlags, command_pool_flags()),
  create_command_pool_nif(Dev, CreateInfo#vk_command_pool_create_info{flags = IntFlags}).

-spec create_command_pool_nif(vk_device(), vk_command_pool_create_info()) -> either(vk_command_pool(), atom()).
create_command_pool_nif(_Dev, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

-spec destroy_command_pool(vk_device(), vk_command_pool()) -> ok.
destroy_command_pool(_Device, _Pool) -> erlang:nif_error({error, not_loaded}).

-spec create_buffer_flags() -> proplists:proplist().
create_buffer_flags() ->
  [{sparse_binding, 16#1}
   ,{sparse_residency, 16#2}
   ,{sparse_aliased, 16#4}
   ,{protected, 16#8}
  ].

-spec create_buffer_usage_flags() -> proplists:proplist().
create_buffer_usage_flags() ->
  [{transfer_src, 16#001}
   ,{transfer_dst, 16#002}
   ,{uniform_texel_buffer, 16#004}
   ,{storage_texel_buffer, 16#008}
   ,{uniform_buffer, 16#010}
   ,{storage_buffer, 16#020}
   ,{index_buffer, 16#040}
   ,{vertex_buffer, 16#080}
   ,{indirect_buffer, 16#100}
  ].

-spec create_buffer(vk_device(), vk_buffer_create_info()) -> either(vk_buffer(), atom()).
create_buffer(Device, #vk_buffer_create_info{flags = ListFlags, usage = UsageFlags, sharing_mode = Mode} = CreateInfo) ->
  IntFlags = plain_vulkan_util:fold_flags(ListFlags, create_buffer_flags()),

  IntUsage = plain_vulkan_util:fold_flags(UsageFlags, create_buffer_usage_flags()),

  SharingMode = case Mode of
                  [concurrent] -> 1;
                  _ -> 0
                end,
  create_buffer_nif(Device, CreateInfo#vk_buffer_create_info{flags = IntFlags, usage = IntUsage, sharing_mode = SharingMode}).


-spec create_buffer_nif(vk_device(), vk_buffer_create_info()) -> either(vk_buffer(), atom()).
create_buffer_nif(_Device, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

-spec destroy_buffer(vk_device(), vk_buffer()) -> ok.
destroy_buffer(_Device, _Buffer) -> erlang:nif_error({error, not_loaded}).

-spec get_buffer_memory_requirements_nif(vk_device(), vk_buffer()) -> vk_memory_requirements().
get_buffer_memory_requirements_nif(_Device, _Buffer) -> erlang:nif_error({error, not_loaded}).

-spec get_buffer_memory_requirements(vk_device(), vk_buffer()) -> vk_memory_requirements().
get_buffer_memory_requirements(Device, Buffer) ->
  Reqs = get_buffer_memory_requirements_nif(Device, Buffer),
  #vk_memory_requirements{memory_type_flags = Bits} = Reqs,
  Reqs#vk_memory_requirements{memory_type_flags = plain_vulkan_util:unfold_flags(Bits, memory_requirements_flags())}.

-spec memory_requirements_flags() -> proplists:proplist().
memory_requirements_flags() -> memory_types_flags().

-spec allocate_memory(vk_device(), vk_memory_allocate_info()) -> either(vk_device_memory(), atom()).
allocate_memory(_Device, _AllocateInfo) -> erlang:nif_error({error, not_loaded}).

-spec free_memory(vk_device(), vk_device_memory()) -> ok.
free_memory(_Device, _Memory) -> erlang:nif_error({error, not_loaded}).

-spec bind_buffer_memory(vk_device(), vk_buffer(), vk_device_memory(), non_neg_integer()) -> ok | {error, atom()}.
bind_buffer_memory(_Device, _Buffer, _Memory, _Offset) -> erlang:nif_error({error, not_loaded}).

-spec shader_stage_flags() -> proplists:proplist().
shader_stage_flags() ->
  [{vertex, 16#1}
   ,{tesselation_control, 16#2}
   ,{tesselation_evaluation, 16#4}
   ,{geometry, 16#8}
   ,{fragment, 16#10}
   ,{compute, 16#20}
   ,{all_graphics, 16#1F}
   ,{all, 16#7FFFFFFF}
  ].

-spec create_descriptor_set_layout(vk_device(), vk_descriptor_set_layout_create_info()) -> either(vk_descriptor_set_layout(), atom()).
create_descriptor_set_layout(Device, CreateInfo) ->
  Flags = CreateInfo#vk_descriptor_set_layout_create_info.flags,
  Bits = plain_vulkan_util:fold_flags(Flags, shader_stage_flags()),
  create_descriptor_set_layout_nif(Device, CreateInfo#vk_descriptor_set_layout_create_info{flags = Bits}).

-spec create_descriptor_set_layout_nif(vk_device(), vk_descriptor_set_layout_create_info()) -> either(vk_descriptor_set_layout(), atom()).
create_descriptor_set_layout_nif(_Device, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

-spec destroy_descriptor_set_layout(vk_device(), vk_descriptor_set_layout()) -> ok.
destroy_descriptor_set_layout(_Device, _SetLayout) -> erlang:nif_error({error, not_loaded}).

-spec create_descriptor_pool(vk_device(), vk_descriptor_pool_create_info()) -> either(vk_descriptor_pool(), atom()).
create_descriptor_pool(Device, CreateInfo) ->
  Flags = CreateInfo#vk_descriptor_pool_create_info.flags,
  Bits = plain_vulkan_util:fold_flags(Flags, [{free_descriptor_set, 16#1}]),
  create_descriptor_pool_nif(Device, CreateInfo#vk_descriptor_pool_create_info{flags = Bits}).

-spec create_descriptor_pool_nif(vk_device(), vk_descriptor_pool_create_info()) -> either(vk_descriptor_pool(), atom()).
create_descriptor_pool_nif(_Device, _CreateInfo) -> erlang:nif_error({error, not_loaded}).

-spec destroy_descriptor_pool(vk_device(), vk_descriptor_pool()) -> ok.
destroy_descriptor_pool(_Device, _Pool) -> erlang:nif_error({error, not_loaded}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() -> ok.
init() ->
  PrivDir = code:priv_dir(?MODULE),
  ok = erlang:load_nif(filename:join(PrivDir, "plain_vulkan_drv"), 0).
