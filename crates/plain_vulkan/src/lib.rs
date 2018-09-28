extern crate vk_sys;
#[macro_use]
extern crate rustler;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler_codegen;

use rustler::{Env, Term, Error, Encoder};
use rustler::resource::ResourceArc;
use std::ffi::{CString, CStr, NulError};
use std::mem;
use std::ptr::{null, null_mut};

#[link(name = "vulkan")]
extern {
    fn vkCreateInstance(create_info: *const vk_sys::InstanceCreateInfo
                        ,allocator: *const vk_sys::AllocationCallbacks
                        ,instance: *mut vk_sys::Instance
    ) -> vk_sys::Result;

    fn vkDestroyInstance(instance: vk_sys::Instance
                         ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkEnumeratePhysicalDevices(instance: vk_sys::Instance
                                  ,physical_device_count: *mut u32
                                  ,physical_devices: *mut vk_sys::PhysicalDevice
    ) -> vk_sys::Result;

    fn vkGetPhysicalDeviceProperties(physical_device: vk_sys::PhysicalDevice
                                     ,properties: *mut vk_sys::PhysicalDeviceProperties
    );

    fn vkGetPhysicalDeviceQueueFamilyProperties(physical_device: vk_sys::PhysicalDevice
                                                ,queue_family_property_count: *mut u32
                                                ,queue_family_properties: *mut vk_sys::QueueFamilyProperties
    );

    fn vkCreateDevice(physical_device: vk_sys::PhysicalDevice
                      ,create_info: *const vk_sys::DeviceCreateInfo
                      ,allocator: *const vk_sys::AllocationCallbacks
                      ,device: *mut vk_sys::Device
    ) -> vk_sys::Result;

    fn vkDeviceWaitIdle(device: vk_sys::Device) -> vk_sys::Result;

    fn vkDestroyDevice(device: vk_sys::Device
                       ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkGetDeviceQueue(device: vk_sys::Device
                        ,queue_family_index: u32
                        ,queue_index: u32
                        ,queue: *mut vk_sys::Queue
    );

    fn vkCreateCommandPool(device: vk_sys::Device
                           ,create_info: *const vk_sys::CommandPoolCreateInfo
                           ,allocator: *const vk_sys::AllocationCallbacks
                           ,command_pool: *mut vk_sys::CommandPool
    ) -> vk_sys::Result;

    fn vkDestroyCommandPool(device: vk_sys::Device
                            ,command_pool: vk_sys::CommandPool
                            ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkCreateBuffer(device: vk_sys::Device
                      ,create_info: *const vk_sys::BufferCreateInfo
                      ,allocator: *const vk_sys::AllocationCallbacks
                      ,buffer: *mut vk_sys::Buffer
    ) -> vk_sys::Result;

    fn vkDestroyBuffer(device: vk_sys::Device
                       ,buffer: vk_sys::Buffer
                       ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkGetBufferMemoryRequirements(device: vk_sys::Device
                                     ,buffer: vk_sys::Buffer
                                     ,memory_requirements: *mut vk_sys::MemoryRequirements
    );

    fn vkGetPhysicalDeviceMemoryProperties(device: vk_sys::PhysicalDevice
                                           ,memory_properties: *mut vk_sys::PhysicalDeviceMemoryProperties
    );

    fn vkAllocateMemory(device: vk_sys::Device
                        ,allocate_info: *const vk_sys::MemoryAllocateInfo
                        ,allocator: *const vk_sys::AllocationCallbacks
                        ,memory: *mut vk_sys::DeviceMemory
    ) -> vk_sys::Result;

    fn vkFreeMemory(device: vk_sys::Device
                    ,memory: vk_sys::DeviceMemory
                    ,allocator: *const vk_sys::AllocationCallbacks
    );
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom undefined;
        atom nif_error;

        // Vulkan
        atom incomplete;
        atom out_of_host_memory;
        atom out_of_device_memory;
        atom initialization_failed;
        atom layers_not_present;
        atom extension_not_present;
        atom incompatible_driver;
        atom too_many_objects;

        // records
        atom vk_physical_device_properties;
        atom vk_extent_3d;
        atom vk_queue_family_properties;
    }
}

#[derive(NifRecord)]
#[tag="vk_physical_device_features"]
struct ErlVkPhysicalDeviceFeatures {
    pub robust_buffer_access: bool, pub full_draw_index_uint32: bool, pub image_cube_array: bool,
    pub independent_blend: bool, pub geometry_shader: bool, pub tessellation_shader: bool,
    pub sample_rate_shading: bool, pub dual_src_blend: bool, pub logic_op: bool,
    pub multi_draw_indirect: bool, pub draw_indirect_first_instance: bool, pub depth_clamp: bool,
    pub depth_bias_clamp: bool, pub fill_mode_non_solid: bool, pub depth_bounds: bool,
    pub wide_lines: bool, pub large_points: bool, pub alpha_to_one: bool,
    pub multi_viewport: bool, pub sampler_anisotropy: bool, pub texture_compression_etc2: bool,
    pub texture_compression_astc_ldr: bool, pub texture_compression_bc: bool,
    pub occlusion_query_precise: bool, pub pipeline_statistics_query: bool,
    pub vertex_pipeline_stores_and_atomics: bool, pub fragment_stores_and_atomics: bool,
    pub shader_tessellation_and_geometry_point_size: bool, pub shader_image_gather_extended: bool,
    pub shader_storage_image_extended_formats: bool, pub shader_storage_image_multisample: bool,
    pub shader_storage_image_read_without_format: bool, pub shader_storage_image_write_without_format: bool,
    pub shader_uniform_buffer_array_dynamic_indexing: bool, pub shader_sampled_image_array_dynamic_indexing: bool,
    pub shader_storage_buffer_array_dynamic_indexing: bool, pub shader_storage_image_array_dynamic_indexing: bool,
    pub shader_clip_distance: bool, pub shader_cull_distance: bool, pub shader_float64: bool,
    pub shader_int64: bool, pub shader_int16: bool, pub shader_resource_residency: bool,
    pub shader_resource_min_lod: bool, pub sparse_binding: bool, pub sparse_residency_buffer: bool,
    pub sparse_residency_image_2d: bool, pub sparse_residency_image_3d: bool,
    pub sparse_residency2_samples: bool, pub sparse_residency4_samples: bool,
    pub sparse_residency8_samples: bool, pub sparse_residency16_samples: bool,
    pub sparse_residency_aliased: bool, pub variable_multisample_rate: bool, pub inherited_queries: bool
}

#[derive(NifRecord)]
#[tag="vk_device_queue_create_info"]
struct ErlVkDeviceQueueCreateInfo {
    pub queue_family_index : u32,
    pub queue_count : u32,
    pub queue_priorities : Vec<f32>
}

#[derive(NifRecord)]
#[tag="vk_device_create_info"]
struct ErlVkDeviceCreateInfo {
    pub queue_create_infos : Vec<ErlVkDeviceQueueCreateInfo>,
    pub enabled_features : ErlVkPhysicalDeviceFeatures
}

#[derive(NifRecord)]
#[tag="vk_command_pool_create_info"]
struct ErlVkCommandPoolCreateInfo {
    flags: vk_sys::CommandPoolCreateFlags,
    queue_family_index: u32
}

#[derive(NifRecord)]
#[tag="vk_buffer_create_info"]
struct ErlVkBufferCreateInfo {
    flags: vk_sys::BufferCreateFlags,
    size: vk_sys::DeviceSize,
    usage: vk_sys::BufferUsageFlags,
    sharing_mode: vk_sys::SharingMode,
    queue_family_indices: Vec<u32>
}

#[derive(NifRecord)]
#[tag="vk_memory_requirements"]
struct ErlVkMemoryRequirements {
    size: vk_sys::DeviceSize,
    alignment: vk_sys::DeviceSize,
    memory_type_bits: u32
}

#[derive(NifTuple)]
struct ErlVkMemoryType {
    property_flags: vk_sys::MemoryPropertyFlags,
    heap_index: u32
}

#[derive(NifTuple)]
struct ErlVkMemoryHeap {
    size: vk_sys::DeviceSize,
    flags: vk_sys::MemoryHeapFlags
}

#[derive(NifRecord)]
#[tag="vk_physical_device_memory_properties"]
struct ErlVkPhysicalDeviceMemoryProperties {
    memory_types: Vec<ErlVkMemoryType>,
    memory_heaps: Vec<ErlVkMemoryHeap>
}

#[derive(NifRecord)]
#[tag="vk_memory_allocate_info"]
struct ErlVkMemoryAllocateInfo {
    size: vk_sys::DeviceSize,
    memory_type: u32
}

rustler_export_nifs!(
    "plain_vulkan",
    [("create_instance", 1, create_instance_nif)
    ,("destroy_instance", 1, destroy_instance_nif)
    ,("count_physical_devices", 1, count_physical_devices_nif)
    ,("enumerate_physical_devices", 2, enumerate_physical_devices_nif)
    ,("get_physical_device_properties", 1, get_physical_device_properties_nif)
    ,("get_physical_device_queue_family_properties_nif", 2, get_physical_device_queue_family_properties_nif)
    ,("get_physical_device_queue_family_count", 1, get_physical_device_queue_family_count_nif)
    ,("create_device", 2, create_device_nif)
    ,("device_wait_idle", 1, device_wait_idle_nif)
    ,("destroy_device", 1, destroy_device_nif)
    ,("get_device_queue", 3, get_device_queue_nif)
    ,("create_command_pool_nif", 2, create_command_pool_nif)
    ,("destroy_command_pool", 2, destroy_command_pool_nif)
    ,("create_buffer_nif", 2, create_buffer_nif)
    ,("destroy_buffer", 2, destroy_buffer_nif)
    ,("get_buffer_memory_requirements_nif", 2, get_buffer_memory_requirements_nif)
    ,("get_physical_device_memory_properties_nif", 1, get_physical_device_memory_properties_nif)
    ,("allocate_memory", 2, allocate_memory_nif)
    ,("free_memory", 2, free_memory_nif)
    ],
    Some(on_load)
);

struct InstanceHolder {
    pub value : vk_sys::Instance
}

struct DeviceHolder {
    pub value : vk_sys::Device
}

struct QueueHolder {
    pub value : vk_sys::Queue
}

struct CommandPoolHolder {
    pub value : vk_sys::CommandPool
}

struct BufferHolder {
    pub value: vk_sys::Buffer
}

struct DeviceMemoryHolder {
    pub value: vk_sys::DeviceMemory
}

fn on_load(env: Env, _info: Term) -> bool {
    resource_struct_init!(InstanceHolder, env);
    resource_struct_init!(DeviceHolder, env);
    resource_struct_init!(QueueHolder, env);
    resource_struct_init!(CommandPoolHolder, env);
    resource_struct_init!(BufferHolder, env);
    resource_struct_init!(DeviceMemoryHolder, env);
    true
}

#[inline]
fn vk_make_version(major:u32, minor: u32, patch: u32) -> u32 {
    ((major) << 22) | ((minor) << 12) | (patch)
}

#[inline]
fn erl_ok<'a, T: Encoder>(env: Env<'a>, data: T) -> Term<'a> {
    (atoms::ok(), data).encode(env)
}

#[inline]
fn erl_error<'a, T: Encoder>(env: Env<'a>, data: T) -> Term<'a> {
    (atoms::error(), data).encode(env)
}

#[inline]
fn match_return<'a, T: Encoder>(env: Env<'a>, result: vk_sys::Result, data: T) -> Result<Term<'a>, Error> {
    match result {
        vk_sys::SUCCESS =>
            Ok(erl_ok(env, data.encode(env))),
        vk_sys::INCOMPLETE =>
            Ok((atoms::incomplete(), data).encode(env)),
        vk_sys::ERROR_OUT_OF_HOST_MEMORY =>
            Ok(erl_error(env, atoms::out_of_host_memory())),
        vk_sys::ERROR_OUT_OF_DEVICE_MEMORY =>
            Ok(erl_error(env, atoms::out_of_device_memory())),
        vk_sys::ERROR_INITIALIZATION_FAILED =>
            Ok(erl_error(env, atoms::initialization_failed())),
        vk_sys::ERROR_LAYER_NOT_PRESENT =>
            Ok(erl_error(env, atoms::layers_not_present())),
        vk_sys::ERROR_EXTENSION_NOT_PRESENT =>
            Ok(erl_error(env, atoms::extension_not_present())),
        vk_sys::ERROR_INCOMPATIBLE_DRIVER =>
            Ok(erl_error(env, atoms::incompatible_driver())),
        vk_sys::ERROR_TOO_MANY_OBJECTS =>
            Ok(erl_error(env, atoms::too_many_objects())),
        _ =>
            Ok(erl_error(env, atoms::nif_error()))
    }

}

fn create_instance_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let erl_app_name:&str = args[0].decode()?;
    let app_name = match CString::new(erl_app_name) {
        Ok(string) => string,
        Err(NulError{..}) => return Err(Error::Atom("nif_error"))
    };

    let app_info = vk_sys::ApplicationInfo {
        sType : vk_sys::STRUCTURE_TYPE_APPLICATION_INFO,
        pNext: null(),
        pApplicationName: app_name.as_ptr(),
        applicationVersion: vk_make_version(0, 1,0),
        pEngineName: app_name.as_ptr(),
        engineVersion: vk_make_version(0, 1, 0),
        apiVersion: vk_make_version(1, 0, 0),
    };

    let create_info = vk_sys::InstanceCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        pNext: null(),
        flags: 0,
        pApplicationInfo: &app_info,
        enabledLayerCount: 0,
        ppEnabledLayerNames: null(),
        enabledExtensionCount: 0,
        ppEnabledExtensionNames: null(),
    };

    let (result, instance) = unsafe {
        let mut holder = InstanceHolder {
            value: mem::uninitialized()
        };
        let vk_result = vkCreateInstance(&create_info, null(), &mut holder.value);
        (vk_result, holder)
    };

    match_return(env, result, ResourceArc::new(instance))
}

fn destroy_instance_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder : ResourceArc<InstanceHolder> = args[0].decode()?;

    unsafe {
        vkDestroyInstance(inst_holder.value, null());
    }

    Ok(atoms::ok().encode(env))
}

fn count_physical_devices_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder: ResourceArc<InstanceHolder> = args[0].decode()?;

    let (result, count) = unsafe {
        let mut c:u32 = 0;
        let r: vk_sys::Result;
        r = vkEnumeratePhysicalDevices(inst_holder.value, &mut c, null_mut());
        (r, c)
    };

    match_return(env, result, count)
}

fn enumerate_physical_devices_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder: ResourceArc<InstanceHolder> = args[0].decode()?;
    let mut count: u32 = args[1].decode()?;

    let (result, devices) = unsafe {
        let r: vk_sys::Result;
        let mut pdevices: Vec<vk_sys::PhysicalDevice> = vec![mem::zeroed(); count as usize];
        r = vkEnumeratePhysicalDevices(inst_holder.value, &mut count, pdevices.as_mut_ptr());
        (r, pdevices)
    };

    match_return(env, result, devices)
}

fn get_physical_device_properties_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let physical_device: vk_sys::PhysicalDevice = args[0].decode()?;
    let props = unsafe {
        let mut p: vk_sys::PhysicalDeviceProperties = mem::zeroed();

        vkGetPhysicalDeviceProperties(physical_device, &mut p);
        p
    };

    let dev_name = match unsafe {CStr::from_ptr(props.deviceName.as_ptr())}.to_str() {
        Ok(string) => string,
        Err(_) => return Err(Error::Atom("nif_error"))
    };

    let cache_uuid = props.pipelineCacheUUID.as_ref();

    Ok((atoms::vk_physical_device_properties()
        ,props.apiVersion
        ,props.driverVersion
        ,props.vendorID
        ,props.deviceID
        ,props.deviceType
        ,dev_name
        ,cache_uuid
        ,atoms::undefined()
        ,atoms::undefined()
    ).encode(env))

}

fn get_physical_device_queue_family_count_nif<'a>(env: Env<'a>, args: &[Term<'a>])
    -> Result<Term<'a>, Error>
{
    let physical_device: vk_sys::PhysicalDevice = args[0].decode()?;
    let count = unsafe {
        let mut c = 0;
        vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &mut c, null_mut());
        c
    };

    Ok(erl_ok(env, count))
}

fn get_physical_device_queue_family_properties_nif<'a>(env: Env<'a>, args: &[Term<'a>])
                                                       -> Result<Term<'a>, Error>
{
    let physical_device: vk_sys::PhysicalDevice = args[0].decode()?;
    let count:u32 = args[1].decode()?;

    let props = unsafe {
        let mut p = Vec::with_capacity(count as usize);
        let mut c = count;
        vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &mut c, p.as_mut_ptr());
        p.set_len(c as usize);
        p
    };

    let mut result:Vec<Term> = Vec::new();
    for (idx, prop) in props.iter().enumerate() {
        let extent = &prop.minImageTransferGranularity;
        let erl_extent = (atoms::vk_extent_3d()
                          ,extent.width
                          ,extent.height
                          ,extent.depth
        );
        let erl_property = (atoms::vk_queue_family_properties()
                            ,prop.queueFlags
                            ,prop.queueCount
                            ,prop.timestampValidBits
                            ,erl_extent
                            ,idx
        );
        result.push(erl_property.encode(env));
    };

    Ok(erl_ok(env, result.encode(env)))
}

fn create_device_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let physical_device: vk_sys::PhysicalDevice = args[0].decode()?;
    let device_create_info : ErlVkDeviceCreateInfo = args[1].decode()?;

    let device_features = vk_sys::PhysicalDeviceFeatures {
        robustBufferAccess : device_create_info.enabled_features.robust_buffer_access as u32,
        fullDrawIndexUint32 : device_create_info.enabled_features.full_draw_index_uint32 as u32,
        imageCubeArray : device_create_info.enabled_features.image_cube_array as u32,
        independentBlend : device_create_info.enabled_features.independent_blend as u32,
        geometryShader : device_create_info.enabled_features.geometry_shader as u32,
        tessellationShader : device_create_info.enabled_features.tessellation_shader as u32,
        sampleRateShading : device_create_info.enabled_features.sample_rate_shading as u32,
        dualSrcBlend : device_create_info.enabled_features.dual_src_blend as u32,
        logicOp : device_create_info.enabled_features.logic_op as u32,
        multiDrawIndirect : device_create_info.enabled_features.multi_draw_indirect as u32,
        drawIndirectFirstInstance : device_create_info.enabled_features.draw_indirect_first_instance as u32,
        depthClamp : device_create_info.enabled_features.depth_clamp as u32,
        depthBiasClamp : device_create_info.enabled_features.depth_bias_clamp as u32,
        fillModeNonSolid : device_create_info.enabled_features.fill_mode_non_solid as u32,
        depthBounds : device_create_info.enabled_features.depth_bounds as u32,
        wideLines : device_create_info.enabled_features.wide_lines as u32,
        largePoints : device_create_info.enabled_features.large_points as u32,
        alphaToOne : device_create_info.enabled_features.alpha_to_one as u32,
        multiViewport : device_create_info.enabled_features.multi_viewport as u32,
        samplerAnisotropy : device_create_info.enabled_features.sampler_anisotropy as u32,
        textureCompressionETC2 : device_create_info.enabled_features.texture_compression_etc2 as u32,
        textureCompressionASTC_LDR : device_create_info.enabled_features.texture_compression_astc_ldr as u32,
        textureCompressionBC : device_create_info.enabled_features.texture_compression_bc as u32,
        occlusionQueryPrecise : device_create_info.enabled_features.occlusion_query_precise as u32,
        pipelineStatisticsQuery : device_create_info.enabled_features.pipeline_statistics_query as u32,
        vertexPipelineStoresAndAtomics : device_create_info.enabled_features.vertex_pipeline_stores_and_atomics as u32,
        fragmentStoresAndAtomics : device_create_info.enabled_features.fragment_stores_and_atomics as u32,
        shaderTessellationAndGeometryPointSize : device_create_info.enabled_features.shader_tessellation_and_geometry_point_size as u32,
        shaderImageGatherExtended : device_create_info.enabled_features.shader_image_gather_extended as u32,
        shaderStorageImageExtendedFormats : device_create_info.enabled_features.shader_storage_image_extended_formats as u32,
        shaderStorageImageMultisample : device_create_info.enabled_features.shader_storage_image_multisample as u32,
        shaderStorageImageReadWithoutFormat : device_create_info.enabled_features.shader_storage_image_read_without_format as u32,
        shaderStorageImageWriteWithoutFormat : device_create_info.enabled_features.shader_storage_image_write_without_format as u32,
        shaderUniformBufferArrayDynamicIndexing : device_create_info.enabled_features.shader_uniform_buffer_array_dynamic_indexing as u32,
        shaderSampledImageArrayDynamicIndexing : device_create_info.enabled_features.shader_sampled_image_array_dynamic_indexing as u32,
        shaderStorageBufferArrayDynamicIndexing : device_create_info.enabled_features.shader_storage_buffer_array_dynamic_indexing as u32,
        shaderStorageImageArrayDynamicIndexing : device_create_info.enabled_features.shader_storage_image_array_dynamic_indexing as u32,
        shaderClipDistance : device_create_info.enabled_features.shader_clip_distance as u32,
        shaderCullDistance : device_create_info.enabled_features.shader_cull_distance as u32,
        shaderf3264 : device_create_info.enabled_features.shader_float64 as u32,
        shaderInt64 : device_create_info.enabled_features.shader_int64 as u32,
        shaderInt16 : device_create_info.enabled_features.shader_int16 as u32,
        shaderResourceResidency : device_create_info.enabled_features.shader_resource_residency as u32,
        shaderResourceMinLod : device_create_info.enabled_features.shader_resource_min_lod as u32,
        sparseBinding : device_create_info.enabled_features.sparse_binding as u32,
        sparseResidencyBuffer : device_create_info.enabled_features.sparse_residency_buffer as u32,
        sparseResidencyImage2D : device_create_info.enabled_features.sparse_residency_image_2d as u32,
        sparseResidencyImage3D : device_create_info.enabled_features.sparse_residency_image_3d as u32,
        sparseResidency2Samples : device_create_info.enabled_features.sparse_residency2_samples as u32,
        sparseResidency4Samples : device_create_info.enabled_features.sparse_residency4_samples as u32,
        sparseResidency8Samples : device_create_info.enabled_features.sparse_residency8_samples as u32,
        sparseResidency16Samples : device_create_info.enabled_features.sparse_residency16_samples as u32,
        sparseResidencyAliased : device_create_info.enabled_features.sparse_residency_aliased as u32,
        variableMultisampleRate : device_create_info.enabled_features.variable_multisample_rate as u32,
        inheritedQueries : device_create_info.enabled_features.inherited_queries as u32
    };
    let len = device_create_info.queue_create_infos.len();
    let mut queue_create_infos : Vec<vk_sys::DeviceQueueCreateInfo> = Vec::new();

    unsafe {
        queue_create_infos.reserve(len);
        queue_create_infos.set_len(len);
    };

    for idx in 0..device_create_info.queue_create_infos.len() {
        queue_create_infos[idx].sType = vk_sys::STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queue_create_infos[idx].flags = 0;
        queue_create_infos[idx].pNext = null();
        queue_create_infos[idx].queueFamilyIndex = device_create_info.queue_create_infos[idx].queue_family_index;
        queue_create_infos[idx].queueCount = device_create_info.queue_create_infos[idx].queue_count;
        queue_create_infos[idx].pQueuePriorities = device_create_info.queue_create_infos[idx].queue_priorities.as_ptr();
    }

    let create_info = unsafe {
        let mut unsafe_create_info : vk_sys::DeviceCreateInfo = std::mem::zeroed();
        unsafe_create_info.sType = vk_sys::STRUCTURE_TYPE_DEVICE_CREATE_INFO;
        unsafe_create_info.queueCreateInfoCount = len as u32;
        unsafe_create_info.pQueueCreateInfos = queue_create_infos.as_ptr();
        unsafe_create_info.pEnabledFeatures = &device_features;

        unsafe_create_info
    };

    let (res, device) = unsafe {
        let mut unsafe_device : DeviceHolder = mem::uninitialized();
        let result = vkCreateDevice(physical_device, &create_info, null(), &mut unsafe_device.value);
        (result, unsafe_device)
    };

    match_return(env, res, ResourceArc::new(device))
}

fn device_wait_idle_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;

    unsafe {
        vkDeviceWaitIdle(logi_device.value);
    }

    Ok(atoms::ok().encode(env))
}

fn destroy_device_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;

    unsafe {
        vkDestroyDevice(logi_device.value, null())
    };

    Ok(atoms::ok().encode(env))
}

fn get_device_queue_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let queue_family: u32 = args[1].decode()?;
    let queue_index: u32 = args[2].decode()?;

    let queue = unsafe {
        let mut q:QueueHolder = mem::uninitialized();
        vkGetDeviceQueue(logi_device.value, queue_family, queue_index, &mut q.value);
        q
    };

    Ok(ResourceArc::new(queue).encode(env))
}

fn create_command_pool_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_create_info: ErlVkCommandPoolCreateInfo = args[1].decode()?;

    let create_info = vk_sys::CommandPoolCreateInfo{
        sType: vk_sys::STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        pNext: null(),
        flags: erl_create_info.flags,
        queueFamilyIndex: erl_create_info.queue_family_index
    };

    let (result, command_pool) = unsafe {
        let mut pool : CommandPoolHolder = mem::uninitialized();

        let r = vkCreateCommandPool(logi_device.value, &create_info, null(), &mut pool.value);
        (r, pool)
    };

    match_return(env, result, ResourceArc::new(command_pool))
}

fn destroy_command_pool_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let pool: ResourceArc<CommandPoolHolder> = args[1].decode()?;

    unsafe {
        vkDestroyCommandPool(logi_device.value, pool.value, null());
    }

    Ok(atoms::ok().encode(env))
}

fn create_buffer_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_create_info: ErlVkBufferCreateInfo = args[1].decode()?;

    let create_info = vk_sys::BufferCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        pNext: null(),
        flags: erl_create_info.flags,
        size: erl_create_info.size,
        usage: erl_create_info.usage,
        sharingMode: erl_create_info.sharing_mode,
        queueFamilyIndexCount: erl_create_info.queue_family_indices.len() as u32,
        pQueueFamilyIndices: erl_create_info.queue_family_indices.as_ptr()
    };

    let (result, buffer) = unsafe {
        let mut b: BufferHolder = mem::uninitialized();
        let r = vkCreateBuffer(logi_device.value, &create_info, null(), &mut b.value);
        (r, b)
    };

    match_return(env, result, ResourceArc::new(buffer))
}

fn destroy_buffer_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let buffer: ResourceArc<BufferHolder> = args[1].decode()?;

    unsafe {
        vkDestroyBuffer(logi_device.value, buffer.value, null());
    };

    Ok(atoms::ok().encode(env))
}

fn get_buffer_memory_requirements_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let buffer: ResourceArc<BufferHolder> = args[1].decode()?;

    let mem_req = unsafe {
        let mut m = mem::uninitialized();

        vkGetBufferMemoryRequirements(logi_device.value, buffer.value, &mut m);
        m
    };

    let erl_ret = ErlVkMemoryRequirements {
        size: mem_req.size,
        alignment: mem_req.alignment,
        memory_type_bits: mem_req.memoryTypeBits
    };

    Ok(erl_ret.encode(env))
}

fn get_physical_device_memory_properties_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let phy_dev: vk_sys::PhysicalDevice = args[0].decode()?;

    let mem_props = unsafe {
        let mut m = mem::uninitialized();
        vkGetPhysicalDeviceMemoryProperties(phy_dev, &mut m);
        m
    };

    let tc = mem_props.memoryTypeCount as usize;
    let hc = mem_props.memoryHeapCount as usize;

    let hs: Vec<ErlVkMemoryHeap> = mem_props.memoryHeaps[0..hc].iter().map(
        |h| ErlVkMemoryHeap{size: h.size, flags: h.flags}
    ).collect();
    let ts: Vec<ErlVkMemoryType> = mem_props.memoryTypes[0..tc].iter().map(
        |t| ErlVkMemoryType{heap_index:t.heapIndex, property_flags: t.propertyFlags}
    ).collect();

    let erl_ret = ErlVkPhysicalDeviceMemoryProperties {
        memory_heaps: hs,
        memory_types: ts
    };

    Ok(erl_ret.encode(env))
}

fn allocate_memory_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_alloc_info: ErlVkMemoryAllocateInfo = args[1].decode()?;
    let alloc_info = vk_sys::MemoryAllocateInfo {
        pNext: null(),
        sType: vk_sys::STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        allocationSize: erl_alloc_info.size,
        memoryTypeIndex: erl_alloc_info.memory_type
    };

    let (result, mem) = unsafe {
        let mut m: DeviceMemoryHolder = mem::uninitialized();
        let r = vkAllocateMemory(logi_device.value, &alloc_info, null(), &mut m.value);
        (r, m)
    };

    match_return(env, result, ResourceArc::new(mem))
}

fn free_memory_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let memory: ResourceArc<DeviceMemoryHolder> = args[1].decode()?;

    unsafe {
        vkFreeMemory(logi_device.value, memory.value, null());
    }

    Ok(atoms::ok().encode(env))
}
