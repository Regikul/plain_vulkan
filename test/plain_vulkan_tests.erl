-module(plain_vulkan_tests).

-include_lib("eunit/include/eunit.hrl").

flow_test() ->
  {ok, Instance} = plain_vulkan:create_instance(?MODULE_STRING),
  case plain_vulkan:count_physical_devices(Instance) of
    {ok, _Count} when is_integer(_Count) andalso _Count > 0 -> ok;
    out_of_device_memory -> ok;
    out_of_host_memory -> ok;
    init_failed -> ok
  end,
  {ok, Devices} = plain_vulkan:enumerate_physical_devices(Instance),
  _FirstDeviceProperties = plain_vulkan:get_physical_device_properties(hd(Devices)),
  ok = plain_vulkan:destroy_instance(Instance).
