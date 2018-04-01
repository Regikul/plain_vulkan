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

-record(vk_physical_device_features, {
  robustBufferAccess :: boolean(),
  fullDrawIndexUint32 :: boolean(),
  imageCubeArray :: boolean(),
  independentBlend :: boolean(),
  geometryShader :: boolean(),
  tessellationShader :: boolean(),
  sampleRateShading :: boolean(),
  dualSrcBlend :: boolean(),
  logicOp :: boolean(),
  multiDrawIndirect :: boolean(),
  drawIndirectFirstInstance :: boolean(),
  depthClamp :: boolean(),
  depthBiasClamp :: boolean(),
  fillModeNonSolid :: boolean(),
  depthBounds :: boolean(),
  wideLines :: boolean(),
  largePoints :: boolean(),
  alphaToOne :: boolean(),
  multiViewport :: boolean(),
  samplerAnisotropy :: boolean(),
  textureCompressionETC2 :: boolean(),
  textureCompressionASTC_LDR :: boolean(),
  textureCompressionBC :: boolean(),
  occlusionQueryPrecise :: boolean(),
  pipelineStatisticsQuery :: boolean(),
  vertexPipelineStoresAndAtomics :: boolean(),
  fragmentStoresAndAtomics :: boolean(),
  shaderTessellationAndGeometryPointSize :: boolean(),
  shaderImageGatherExtended :: boolean(),
  shaderStorageImageExtendedFormats :: boolean(),
  shaderStorageImageMultisample :: boolean(),
  shaderStorageImageReadWithoutFormat :: boolean(),
  shaderStorageImageWriteWithoutFormat :: boolean(),
  shaderUniformBufferArrayDynamicIndexing :: boolean(),
  shaderSampledImageArrayDynamicIndexing :: boolean(),
  shaderStorageBufferArrayDynamicIndexing :: boolean(),
  shaderStorageImageArrayDynamicIndexing :: boolean(),
  shaderClipDistance :: boolean(),
  shaderCullDistance :: boolean(),
  shaderFloat64 :: boolean(),
  shaderInt64 :: boolean(),
  shaderInt16 :: boolean(),
  shaderResourceResidency :: boolean(),
  shaderResourceMinLod :: boolean(),
  sparseBinding :: boolean(),
  sparseResidencyBuffer :: boolean(),
  sparseResidencyImage2D :: boolean(),
  sparseResidencyImage3D :: boolean(),
  sparseResidency2Samples :: boolean(),
  sparseResidency4Samples :: boolean(),
  sparseResidency8Samples :: boolean(),
  sparseResidency16Samples :: boolean(),
  sparseResidencyAliased :: boolean(),
  variableMultisampleRate :: boolean(),
  inheritedQueries :: boolean()
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
  minImageTransferGranularity :: #vk_extent_3d{}
}).
-type vk_queue_family_properties() :: #vk_queue_family_properties{}.

-endif.
