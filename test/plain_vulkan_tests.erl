-module(plain_vulkan_tests).

-define(BUFFER_SIZE, 1024).

-include_lib("eunit/include/eunit.hrl").
-include("../include/plain_vulkan.hrl").

flow_test() ->
  AppName = erlang:atom_to_binary(?MODULE, latin1),
  {ok, Instance} = plain_vulkan:create_instance(AppName),
  ok = case plain_vulkan:count_physical_devices(Instance) of
         {ok, _CountDev} when is_integer(_CountDev) andalso _CountDev > 0 -> ok;
         ok -> fail;
         Else -> Else
       end,
  {ok, [PhysDevice | _ ]} = plain_vulkan:enumerate_physical_devices(Instance),
  _DeviceProperties = plain_vulkan:get_physical_device_properties(PhysDevice),
  {ok, QueueFamilyProperties} = plain_vulkan:get_physical_device_queue_family_properties(PhysDevice),
  ComputeQueueInfo = lists:foldl(fun find_compute_queue/2, null, QueueFamilyProperties),
  DeviceCreateInfo = #vk_device_create_info{queue_create_infos = [ComputeQueueInfo]
                                            ,enabled_features = #vk_physical_device_features{}
                                           },
  {ok, Device} = plain_vulkan:create_device(PhysDevice, DeviceCreateInfo),
  #vk_device_queue_create_info{queue_family_index = ComputeFamily} = ComputeQueueInfo,
  ComputeQueue = plain_vulkan:get_device_queue(Device, ComputeFamily, 0),
  true = is_reference(ComputeQueue),

  BufferCreateInfo = #vk_buffer_create_info{queue_family_indices = [ComputeFamily]
                                            ,size = ?BUFFER_SIZE
                                            ,usage = [transfer_src, transfer_dst]
                                           },
  {ok, Buffer} = plain_vulkan:create_buffer(Device, BufferCreateInfo),
  #vk_memory_requirements{size = MemReqSize
                          ,memory_type_flags = MemTypeFlags
                         } = plain_vulkan:get_buffer_memory_requirements(Device, Buffer),
  DeviceMemoryTypes = plain_vulkan:get_physical_device_memory_properties(PhysDevice),
  #vk_physical_device_memory_properties{memory_types = MemTypes} = DeviceMemoryTypes,

  MemoryType = lists:foldl( find_memory_of_type(MemTypeFlags), {0}, MemTypes),

%%  ?debugFmt("required memory is ~pb of type ~p~n", [MemReqSize, MemTypeFlags]),
%%  ?debugFmt("found memory types on this device: ~p~n", [_DeviceMemoryTypes]),
%%  ?debugFmt("selected memory from index ~p", [MemoryType]),

  AllocInfo = #vk_memory_allocate_info{size = MemReqSize, memory_type = MemoryType},

  {ok, Memory} = plain_vulkan:allocate_memory(Device, AllocInfo),
  ok = plain_vulkan:bind_buffer_memory(Device, Buffer, Memory, 0),

  Binding = #vk_descriptor_set_layout_binding{
    binding = 0,
    descriptor_type = ?VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
    descriptor_count = 1,
    stage_flags = [compute]
  },
  DescriptorSetCreateInfo = #vk_descriptor_set_layout_create_info{bindings = [Binding]},
  {ok, Layout} = plain_vulkan:create_descriptor_set_layout(Device, DescriptorSetCreateInfo),
  PoolSize = #vk_descriptor_pool_size{type = ?VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, descriptor_count = 1},
  PoolCreateInfo = #vk_descriptor_pool_create_info{max_sets = 1, pool_sizes = [PoolSize]},
  {ok, Pool} = plain_vulkan:create_descriptor_pool(Device, PoolCreateInfo),
  SetsCreateInfo = #vk_descriptor_set_allocate_info{pool = Pool, set_layouts = [Layout]},
  {ok, [Set]} = plain_vulkan:allocate_descriptor_sets(Device, SetsCreateInfo),

  BufferInfo = #vk_descriptor_buffer_info{buffer = Buffer, offset = 0, range = ?BUFFER_SIZE},
  Writes = #vk_write_descriptor_set{
    dst_set = Set,
    dst_binding = 0,
    dst_array_element = 0,
    descriptor_type = ?VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
    buffer_info = [BufferInfo]
  },
  ok = plain_vulkan:update_descriptor_sets(Device, [Writes], []),

  Priv = code:priv_dir(plain_vulkan),
  {ok, ShaderContents} = file:read_file(filename:join([Priv, "spirv", "shader.comp.spv"])),
  ShaderCreateInfo = #vk_shader_module_create_info{code = binary_to_list(ShaderContents)},
  {ok, ShaderModule} = plain_vulkan:create_shader_module(Device, ShaderCreateInfo),

  PipelineLayoutCreateInfo = #vk_pipeline_layout_create_info{set_layouts = [Layout]
                                                             ,push_constant_ranges = []
                                                            },
  {ok, PipelineLayout} = plain_vulkan:create_pipeline_layout(Device, PipelineLayoutCreateInfo),

  ShaderStageCI = #vk_pipeline_shader_stage_create_info{
    stage_flags = [compute],
    shader_module = ShaderModule,
    name = "main"
  },
  ComputePipelinesCI = #vk_compute_pipeline_create_info{layout = PipelineLayout,
                                                        stage = ShaderStageCI
                                                       },
  {ok, [Pipeline]} = plain_vulkan:create_compute_pipelines(Device, nil, [ComputePipelinesCI]),

  CommandPoolInfo = #vk_command_pool_create_info{flags = [transient, reset], queue_family_index = ComputeFamily},
  {ok, CommandPool} = plain_vulkan:create_command_pool(Device, CommandPoolInfo),

  CommandBufferAI = #vk_command_buffer_allocate_info{
    level = primary,
    command_buffer_count = 1,
    command_pool = CommandPool
  },
  {ok, [CommandBuffer]} = plain_vulkan:allocate_command_buffers(Device, CommandBufferAI),
  {ok, ok} = plain_vulkan:reset_command_buffer(CommandBuffer, []),

  CommandBufferBI = #vk_command_buffer_begin_info{},
  {ok, ok} = plain_vulkan:begin_command_buffer(CommandBuffer, CommandBufferBI),
  {ok, ok} = plain_vulkan:end_command_buffer(CommandBuffer),

  %% --------------- Resource release start here

  ok = plain_vulkan:free_command_buffers(Device, CommandPool, [CommandBuffer]),
  ok = plain_vulkan:destroy_command_pool(Device, CommandPool),
  ok = plain_vulkan:destroy_pipeline(Device, Pipeline),
  ok = plain_vulkan:destroy_pipeline_layout(Device, PipelineLayout),
  ok = plain_vulkan:destroy_shader_module(Device, ShaderModule),
  ok = plain_vulkan:free_descriptor_sets(Device, Pool, [Set]),
  ok = plain_vulkan:destroy_descriptor_pool(Device, Pool),
  ok = plain_vulkan:destroy_descriptor_set_layout(Device, Layout),
  ok = plain_vulkan:free_memory(Device, Memory),
  ok = plain_vulkan:destroy_buffer(Device, Buffer),

  ok = plain_vulkan:device_wait_idle(Device),
  ok = plain_vulkan:destroy_device(Device),
  ok = plain_vulkan:destroy_instance(Instance).

find_memory_of_type(MemReqFlags) ->
  fun ({MemFlags, _HeapNumber}, {Index}) ->
    case lists:subtract(MemReqFlags, MemFlags) of
      [] -> Index;
      _ -> {Index + 1}
    end;
    ({_MemFlags, _HeapNumber}, Index) ->
      Index
  end.

find_compute_queue(#vk_queue_family_properties{queueFlags = Flags, familyIndex = Index}, null) ->
  case lists:member(compute, Flags) of
    'true' -> #vk_device_queue_create_info{queue_count = 1, queue_family_index = Index, queue_priorities = [0.0]};
    'false' -> null
  end;
find_compute_queue(_, #vk_device_queue_create_info{} = Info) ->
  Info.

lists_to_bits_test() ->
  Desc = [{one, 1}, {two, 2}, {four, 4}],
  0 = plain_vulkan_util:fold_flags([], Desc),
  1 = plain_vulkan_util:fold_flags([one], Desc),
  5 = plain_vulkan_util:fold_flags([one, four], Desc),
  3 = plain_vulkan_util:fold_flags([one, two, two], Desc).

bits_to_lists_test() ->
  Desc = [{one, 1}, {two, 2}, {four, 4}],
  [] = plain_vulkan_util:unfold_flags(0, Desc),
  [one] = plain_vulkan_util:unfold_flags(1, Desc),
  List = plain_vulkan_util:unfold_flags(5, Desc),
  true = lists:member(one, List),
  true = lists:member(four, List).
