-ifndef(plain_vulkan_hrl).
-define(plain_vulkan_hrl, 'true').


-define(WORKER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>2000, 'type'=>'worker', 'modules'=>[Name]}).
-define(WORKER(Name, Args), #{'id'=>Name, 'start'=>{Name, 'start_link', [Args]}, 'shutdown'=>2000, 'type'=>'worker', 'modules'=>[Name]}).
-define(SUPER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>'infinity', 'type'=>'supervisor', 'modules'=>[Name]}).

-type terminate_reason() :: 'normal' | 'shutdown' | {'shutdown', term()} | term().
-type child_type() :: 'worker' | 'supervisor'.

%% Don't know if we should implement this
-record(vk_physical_device_limits, {}).
-type vk_physical_device_limits() :: #vk_physical_device_limits{}.

%% Don't know if we should implement this
-record(vk_physical_device_sparce_properties, {}).
-type vk_physical_device_sparce_properties() :: #vk_physical_device_sparce_properties{}.

-record(vk_physical_device_properties,{
  api_version         :: pos_integer(),
  driver_version      :: pos_integer(),
  vendor_id           :: pos_integer(),
  device_id           :: pos_integer(),
  device_type         :: pos_integer(),
  device_name         :: string(),
  pipeline_cache_uuid :: binary(),
  limits              :: vk_physical_device_limits(),             %% not implemented
  sparse_properties   :: vk_physical_device_sparce_properties()   %% not implemented
}).
-type vk_physical_device_properties() :: #vk_physical_device_properties{}.

-endif.
