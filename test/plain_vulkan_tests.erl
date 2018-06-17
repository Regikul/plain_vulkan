-module(plain_vulkan_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/plain_vulkan.hrl").

flow_test() ->
  {ok, Instance} = plain_vulkan:create_instance(?MODULE_STRING),
  ok = case plain_vulkan:count_physical_devices(Instance) of
         {ok, _CountDev} when is_integer(_CountDev) andalso _CountDev > 0 -> ok;
         Else -> Else
       end,
  {ok, [PhysDevice | _ ]} = plain_vulkan:enumerate_physical_devices(Instance),
  _DeviceProperties = plain_vulkan:get_physical_device_properties(PhysDevice),
  _DeviceMemoryTypes = plain_vulkan:get_physical_device_memory_properties(PhysDevice),
  {ok, QueueFamilyProperties} = plain_vulkan:get_physical_device_queue_family_properties(PhysDevice),
  ComputeQueueInfo = lists:foldl(fun find_compute_queue/2, null, QueueFamilyProperties),
  #vk_device_queue_create_info{queue_family_index = ComputeFamily} = ComputeQueueInfo,
  DeviceCreateInfo = #vk_device_create_info{queue_create_infos = [ComputeQueueInfo]
                                            ,enabled_features = #vk_physical_device_features{}
                                           },
  {ok, Device} = plain_vulkan:create_device(PhysDevice, DeviceCreateInfo),
  _ComputeQueue = plain_vulkan:get_device_queue(Device, ComputeFamily, 0),

  CommandPoolInfo = #vk_command_pool_create_info{flags = [transient, reset], queue_family_index = ComputeFamily},
  {ok, CommandPool} = plain_vulkan:create_command_pool(Device, CommandPoolInfo),

  BufferInfo = #vk_buffer_create_info{queue_family_indices = [ComputeFamily]
                                      ,size = 1024
                                      ,usage = [transfer_src, transfer_dst]
                                     },
  {ok, Buffer} = plain_vulkan:create_buffer(Device, BufferInfo),

  ok = plain_vulkan:destroy_buffer(Device, Buffer),
  ok = plain_vulkan:destroy_command_pool(Device, CommandPool),
  ok = plain_vulkan:destroy_device(Device),
  ok = plain_vulkan:destroy_instance(Instance).

find_compute_queue(#vk_queue_family_properties{queueFlags = Flags, familyIndex = Index}, null) ->
  case lists:member(compute, Flags) of
    'true' -> #vk_device_queue_create_info{queue_count = 1, queue_family_index = Index, queue_priorities = [0.0]};
    'false' -> null
  end;
find_compute_queue(_, #vk_device_queue_create_info{} = Info) ->
  Info.
