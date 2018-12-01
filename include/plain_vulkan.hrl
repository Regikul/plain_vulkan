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

-record(vk_memory_requirements, {
  size :: pos_integer(),
  alignment :: non_neg_integer(),
  memory_type_flags :: [atom()]
}).
-type vk_memory_requirements() :: #vk_memory_requirements{}.

-record(vk_memory_allocate_info, {
  size :: pos_integer(),
  memory_type :: non_neg_integer()
}).
-type vk_memory_allocate_info() :: #vk_memory_allocate_info{}.

-type vk_descriptor_type() :: non_neg_integer().
-record(vk_descriptor_set_layout_binding, {
  binding :: non_neg_integer(),
  descriptor_type :: vk_descriptor_type(),
  descriptor_count :: non_neg_integer(),
  stage_flags :: [atom()]
  %% don't want to implement: immutable_samplers
}).
-type vk_descriptor_set_layout_binding() :: vk_descriptor_set_layout_binding().

-record(vk_descriptor_set_layout_create_info, {
  flags = [] :: [atom()],
  bindings :: [vk_descriptor_set_layout_binding()]
}).
-type vk_descriptor_set_layout_create_info() :: #vk_descriptor_set_layout_create_info{}.

-define(VK_DESCRIPTOR_TYPE_SAMPLER,0).
-define(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1).
-define(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,2).
-define(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,3).
-define(VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,4).
-define(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,5).
-define(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,6).
-define(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,7).
-define(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,8).
-define(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,9).
-define(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,10).

-record(vk_descriptor_pool_create_info, {
    flags = [free_descriptor_set] :: atom(),
    max_sets :: non_neg_integer(),
    pool_sizes = [] :: vk_descriptor_pool_size()
}).
-type vk_descriptor_pool_create_info() :: #vk_descriptor_pool_create_info{}.

-record(vk_descriptor_pool_size, {
    type :: vk_descriptor_type(),
    descriptor_count :: non_neg_integer()
}).
-type vk_descriptor_pool_size() :: #vk_descriptor_pool_size{}.

-record (vk_descriptor_set_allocate_info, {
    pool :: plain_vulkan:vk_descriptor_pool(),
    set_layouts :: [plain_vulkan:vk_descriptor_set_layout()]
}).
-type vk_descriptor_set_allocate_info() :: #vk_descriptor_set_allocate_info{}.

-record(vk_descriptor_buffer_info, {
  buffer :: plain_vulkan:vk_buffer(),
  offset :: non_neg_integer(),
  range :: non_neg_integer()
}).
-type vk_descriptor_buffer_info() :: #vk_descriptor_buffer_info{}.

-record(vk_write_descriptor_set, {
  dst_set :: plain_vulkan:vk_descriptor_set(),
  dst_binding :: non_neg_integer(),
  dst_array_element :: non_neg_integer(),
%%  descriptor_count :: non_neg_integer(),              %% don't want to implement
  descriptor_type :: vk_descriptor_type(),
  %%,image_info :: [term()],                            %% don't want to implement
  buffer_info :: [vk_descriptor_buffer_info()]
  %%,texel_biffer_view :: [term()]                      %% don't want to implement
}).
-type vk_write_descriptor_set() :: #vk_write_descriptor_set{}.

-record(vk_copy_descriptor_set, {
  src_set :: plain_vulkan:vk_descriptor_set(),
  src_binding :: non_neg_integer(),
  src_array_element :: non_neg_integer(),
  dst_set :: plain_vulkan:vk_descriptor_set(),
  dst_binding :: non_neg_integer(),
  dst_array_element :: non_neg_integer(),
  descriptor_count :: non_neg_integer()
}).
-type vk_copy_descriptor_set() :: #vk_copy_descriptor_set{}.

-endif.
