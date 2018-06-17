-ifndef(plain_vulkan_hrl).
-define(plain_vulkan_hrl, 'true').


-define(WORKER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>2000, 'type'=>'worker', 'modules'=>[Name]}).
-define(WORKER(Name, Args), #{'id'=>Name, 'start'=>{Name, 'start_link', [Args]}, 'shutdown'=>2000, 'type'=>'worker', 'modules'=>[Name]}).
-define(SUPER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>'infinity', 'type'=>'supervisor', 'modules'=>[Name]}).

-type terminate_reason() :: 'normal' | 'shutdown' | {'shutdown', term()} | term().
-type child_type() :: 'worker' | 'supervisor'.
-type either(Ok, Error) :: {ok, Ok} | {error, Error}.

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

-record(vk_physical_device_features, {
  robustBufferAccess                          = 'false':: boolean(),
  fullDrawIndexUint32                         = 'false':: boolean(),
  imageCubeArray                              = 'false':: boolean(),
  independentBlend                            = 'false':: boolean(),
  geometryShader                              = 'false':: boolean(),
  tessellationShader                          = 'false':: boolean(),
  sampleRateShading                           = 'false':: boolean(),
  dualSrcBlend                                = 'false':: boolean(),
  logicOp                                     = 'false':: boolean(),
  multiDrawIndirect                           = 'false':: boolean(),
  drawIndirectFirstInstance                   = 'false':: boolean(),
  depthClamp                                  = 'false':: boolean(),
  depthBiasClamp                              = 'false':: boolean(),
  fillModeNonSolid                            = 'false':: boolean(),
  depthBounds                                 = 'false':: boolean(),
  wideLines                                   = 'false':: boolean(),
  largePoints                                 = 'false':: boolean(),
  alphaToOne                                  = 'false':: boolean(),
  multiViewport                               = 'false':: boolean(),
  samplerAnisotropy                           = 'false':: boolean(),
  textureCompressionETC2                      = 'false':: boolean(),
  textureCompressionASTC_LDR                  = 'false':: boolean(),
  textureCompressionBC                        = 'false':: boolean(),
  occlusionQueryPrecise                       = 'false':: boolean(),
  pipelineStatisticsQuery                     = 'false':: boolean(),
  vertexPipelineStoresAndAtomics              = 'false':: boolean(),
  fragmentStoresAndAtomics                    = 'false':: boolean(),
  shaderTessellationAndGeometryPointSize      = 'false':: boolean(),
  shaderImageGatherExtended                   = 'false':: boolean(),
  shaderStorageImageExtendedFormats           = 'false':: boolean(),
  shaderStorageImageMultisample               = 'false':: boolean(),
  shaderStorageImageReadWithoutFormat         = 'false':: boolean(),
  shaderStorageImageWriteWithoutFormat        = 'false':: boolean(),
  shaderUniformBufferArrayDynamicIndexing     = 'false':: boolean(),
  shaderSampledImageArrayDynamicIndexing      = 'false':: boolean(),
  shaderStorageBufferArrayDynamicIndexing     = 'false':: boolean(),
  shaderStorageImageArrayDynamicIndexing      = 'false':: boolean(),
  shaderClipDistance                          = 'false':: boolean(),
  shaderCullDistance                          = 'false':: boolean(),
  shaderFloat64                               = 'false':: boolean(),
  shaderInt64                                 = 'false':: boolean(),
  shaderInt16                                 = 'false':: boolean(),
  shaderResourceResidency                     = 'false':: boolean(),
  shaderResourceMinLod                        = 'false':: boolean(),
  sparseBinding                               = 'false':: boolean(),
  sparseResidencyBuffer                       = 'false':: boolean(),
  sparseResidencyImage2D                      = 'false':: boolean(),
  sparseResidencyImage3D                      = 'false':: boolean(),
  sparseResidency2Samples                     = 'false':: boolean(),
  sparseResidency4Samples                     = 'false':: boolean(),
  sparseResidency8Samples                     = 'false':: boolean(),
  sparseResidency16Samples                    = 'false':: boolean(),
  sparseResidencyAliased                      = 'false':: boolean(),
  variableMultisampleRate                     = 'false':: boolean(),
  inheritedQueries                            = 'false':: boolean()
}).
-type vk_physical_device_features() :: #vk_physical_device_features{}.

-record(vk_extent_3d, {
  width :: non_neg_integer(),
  height :: non_neg_integer(),
  depth :: non_neg_integer()
}).
-type vk_extent_3d() :: #vk_extent_3d{}.

-record(vk_queue_family_properties, {
  queueFlags = [] :: [atom()],
  queueCount :: non_neg_integer(),
  timestampValidBits :: non_neg_integer(),
  minImageTransferGranularity :: #vk_extent_3d{},
  familyIndex :: non_neg_integer()
}).
-type vk_queue_family_properties() :: #vk_queue_family_properties{}.

-record(vk_device_queue_create_info, {
  queue_family_index :: non_neg_integer(),
  queue_count :: pos_integer(),
  queue_priorities :: [float()]
}).
-type vk_device_queue_create_info() :: #vk_device_queue_create_info{}.

-record(vk_device_create_info, {
  queue_create_infos = [] :: [vk_device_queue_create_info()],
  enabled_features :: vk_physical_device_features()
}).
-type vk_device_create_info() :: #vk_device_create_info{}.

-record(vk_command_pool_create_info, {
  flags = [] :: list(),
  queue_family_index :: non_neg_integer()
}).
-type vk_command_pool_create_info() :: #vk_command_pool_create_info{}.

-record(vk_buffer_create_info, {
  flags = [] :: list(),
  size :: pos_integer(),
  usage = [] :: list(),
  sharing_mode = [] :: list(),
  queue_family_indices = [] :: list(non_neg_integer())
}).
-type vk_buffer_create_info() :: #vk_buffer_create_info{}.

-type vk_memory_type() :: {[atom()], non_neg_integer()}.
-type vk_memory_heap() :: {non_neg_integer(), [atom()]}.
-record(vk_physical_device_memory_properties,{
  memory_types = [] :: [vk_memory_type()],
  memory_heaps = [] :: [vk_memory_heap()]
}).
-type vk_physical_device_memory_properties() :: #vk_physical_device_memory_properties{}.

-endif.
