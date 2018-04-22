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
  {ok, QueueFamilyProperties} = plain_vulkan:get_physical_device_queue_family_properties(PhysDevice),
  ComputeInfo = #vk_device_queue_create_info{} = lists:foldl(fun find_compute_queue/2, 0, QueueFamilyProperties),
  DeviceCreateInfo = #vk_device_create_info{queue_create_infos = [ComputeInfo]
                                            ,enabled_features = #vk_physical_device_features{}
                                           },
  {ok, Device} = plain_vulkan:create_device(PhysDevice, DeviceCreateInfo),

  plain_vulkan:destroy_device(Device),

  ok = plain_vulkan:destroy_instance(Instance).

find_compute_queue(#vk_queue_family_properties{queueFlags = Flags}, Count) when is_integer(Count) ->
  case lists:member(compute, Flags) of
    'true' -> #vk_device_queue_create_info{queue_count = 1, queue_family_index = Count, queue_priorities = [0.0]};
    'false' -> Count + 1
  end;
find_compute_queue(_, #vk_device_queue_create_info{} = Info) ->
  Info.
