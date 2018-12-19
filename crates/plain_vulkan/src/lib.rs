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

    fn vkBindBufferMemory(device: vk_sys::Device
                          ,buffer: vk_sys::Buffer
                          ,memory: vk_sys::DeviceMemory
                          ,offset: vk_sys::DeviceSize
    ) -> vk_sys::Result;

    fn vkCreateDescriptorSetLayout(device: vk_sys::Device
                                   ,create_info: *const vk_sys::DescriptorSetLayoutCreateInfo
                                   ,allocator: *const vk_sys::AllocationCallbacks
                                   ,set_layout: *mut vk_sys::DescriptorSetLayout
    ) -> vk_sys::Result;

    fn vkDestroyDescriptorSetLayout(device: vk_sys::Device
                                    ,descriptor_set_layout: vk_sys::DescriptorSetLayout
                                    ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkCreateDescriptorPool(device: vk_sys::Device
                              ,create_info: *const vk_sys::DescriptorPoolCreateInfo
                              ,allocator: *const vk_sys::AllocationCallbacks
                              ,descriptor_pool: *mut vk_sys::DescriptorPool
    ) -> vk_sys::Result;

    fn vkDestroyDescriptorPool(device: vk_sys::Device
                               ,descriptor_pool: vk_sys::DescriptorPool
                               ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkAllocateDescriptorSets(device: vk_sys::Device
                                ,allocate_info: *const vk_sys::DescriptorSetAllocateInfo
                                ,descriptor_sets: *mut vk_sys::DescriptorSet
    ) -> vk_sys::Result;

    fn vkFreeDescriptorSets(device: vk_sys::Device
                            ,descriptor_pool: vk_sys::DescriptorPool
                            ,descriptor_set_count: u32
                            ,descriptor_sets: *const vk_sys::DescriptorSet
    ) -> vk_sys::Result;

    fn vkUpdateDescriptorSets(device: vk_sys::Device
                              ,write_count: u32
                              ,writes: *const vk_sys::WriteDescriptorSet
                              ,copy_count: u32
                              ,copies: *const vk_sys::CopyDescriptorSet
    );

    fn vkCreateShaderModule(device: vk_sys::Device
                            ,create_info: *const vk_sys::ShaderModuleCreateInfo
                            ,allocator: *const vk_sys::AllocationCallbacks
                            ,shader_module: *mut vk_sys::ShaderModule
    ) -> vk_sys::Result;

    fn vkDestroyShaderModule(device: vk_sys::Device
                             ,shader_module: vk_sys::ShaderModule
                             ,allocator: *const vk_sys::AllocationCallbacks
    );

    fn vkCreatePipelineLayout(device: vk_sys::Device
                             ,create_info: *const vk_sys::PipelineLayoutCreateInfo
                             ,allocator: *const vk_sys::AllocationCallbacks
                             ,pipeline_layout: *mut vk_sys::PipelineLayout
    ) -> vk_sys::Result;

    fn vkDestroyPipelineLayout(device: vk_sys::Device
                             ,pipeline_layout: vk_sys::PipelineLayout
                             ,allocator: *const vk_sys::AllocationCallbacks
    );

}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
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
        atom device_lost;
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
#[tag="vk_physical_device_properties"]
struct ErlVkPhysicalDeviceProperties {
    pub api_version: u32,
    pub driver_version: u32,
    pub vendor_id: u32,
    pub device_id: u32,
    pub device_type: vk_sys::PhysicalDeviceType,
    pub device_name: String,
    pub pipeline_cache_uuid: Vec<u8>,
    pub nothing1: Option<bool>,
    pub nothing2: Option<bool>
}

#[derive(NifRecord)]
#[tag="vk_extent_3d"]
struct ErlVkExtent3D {
    width: u32,
    height: u32,
    depth: u32
}

#[derive(NifRecord)]
#[tag="vk_queue_family_properties"]
struct ErlVkQueueFamilyProperties {
    queue_flags: vk_sys::QueueFlags,
    queue_count: u32,
    timestamp_valid_bits: u32,
    min_image_transfer_granularity: ErlVkExtent3D,
    family_index: u32
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

#[derive(NifRecord)]
#[tag="vk_descriptor_set_layout_binding"]
struct ErlVkDescriptorSetLayoutBinding {
    binding: u32,
    descriptor_type: vk_sys::DescriptorType,
    descriptor_count: u32,
    stage_flags: vk_sys::ShaderStageFlags
}

#[derive(NifRecord)]
#[tag="vk_descriptor_set_layout_create_info"]
struct ErlVkDescriptorSetLayoutCreateInfo {
    flags: vk_sys::DescriptorSetLayoutCreateFlags,
    bindings: Vec<ErlVkDescriptorSetLayoutBinding>
}

#[derive(NifRecord)]
#[tag="vk_descriptor_pool_size"]
struct ErlVkDescriptorPoolSize {
    descriptor_type: vk_sys::DescriptorType,
    descriptor_count: u32
}

#[derive(NifRecord)]
#[tag="vk_descriptor_pool_create_info"]
struct ErlVkDescriptorPoolCreateInfo {
    flags: vk_sys::DescriptorPoolCreateFlags,
    max_sets: u32,
    pool_sizes: Vec<ErlVkDescriptorPoolSize>
}

#[derive(NifRecord)]
#[tag="vk_descriptor_set_allocate_info"]
struct ErlVkDescriptorSetAllocateInfo {
    pool: ResourceArc<DescriptorPoolHolder>,
    set_layouts: Vec<ResourceArc<DescriptorSetLayoutHolder>>
}

#[derive(NifRecord)]
#[tag="vk_descriptor_buffer_info"]
struct ErlVkDescriptorBufferInfo {
    buffer: ResourceArc<BufferHolder>,
    offset: vk_sys::DeviceSize,
    range: vk_sys::DeviceSize
}

#[derive(NifRecord)]
#[tag="vk_write_descriptor_set"]
struct ErlVkWriteDescriptorSet {
    dst_set: ResourceArc<DescriptorSetHolder>,
    dst_binding: u32,
    dst_array_element: u32,
//    descriptor_count: u32,
    descriptor_type: vk_sys::DescriptorType,
    //,image_info :: [term()],                      %% don't want to implement
     buffer_info: Vec<ErlVkDescriptorBufferInfo>
    // texel_biffer_view :: [term()]                %% don't want to implement
}

#[derive(NifRecord)]
#[tag="vk_copy_descriptor_set"]
struct ErlVkCopyDescriptorSet {
    src_set: ResourceArc<DescriptorSetHolder>,
    src_binding: u32,
    src_array_element: u32,
    dst_set: ResourceArc<DescriptorSetHolder>,
    dst_binding: u32,
    dst_array_element: u32,
    descriptor_count:u32
}

#[derive(NifRecord)]
#[tag="vk_shader_module_create_info"]
struct ErlVkShaderModuleCreateInfo {
    flags: u32,
    code: Vec<u8>
}

#[derive(NifRecord)]
#[tag="vk_push_constant_range"]
struct ErlVkPushConstantRange {
    flags: u32,
    offset: u32,
    size: u32
}

#[derive(NifRecord)]
#[tag="vk_pipeline_layout_create_info"]
struct ErlVkPipelineCreateInfo {
    flags: u32,
    set_layouts: Vec<ResourceArc<DescriptorSetLayoutHolder>>,
    push_constant_ranges: Vec<ErlVkPushConstantRange>
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
    ,("bind_buffer_memory", 4, bind_buffer_memory_nif)
    ,("create_descriptor_set_layout_nif", 2, create_descriptor_set_layout_nif)
    ,("destroy_descriptor_set_layout", 2, destroy_descriptor_set_layout_nif)
    ,("create_descriptor_pool_nif", 2, create_descriptor_pool_nif)
    ,("destroy_descriptor_pool", 2, destroy_descriptor_pool_nif)
    ,("allocate_descriptor_sets", 2, allocate_descriptor_sets_nif)
    ,("free_descriptor_sets", 3, free_descriptor_sets_nif)
    ,("update_descriptor_sets", 3, update_descriptor_sets_nif)
    ,("create_shader_module_nif", 2, create_shader_module_nif)
    ,("destroy_shader_module", 2, destroy_shader_module_nif)
    ,("create_pipeline_layout_nif", 2, create_pipeline_layout_nif)
    ,("destroy_pipeline_layout", 2, destroy_pipeline_layout_nif)
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

struct DescriptorSetLayoutHolder {
    pub value: vk_sys::DescriptorSetLayout
}

struct DescriptorPoolHolder {
    pub value: vk_sys::DescriptorPool
}

struct DescriptorSetHolder {
    pub value: vk_sys::DescriptorSet
}

struct ShaderModuleHolder {
    pub value: vk_sys::ShaderModule
}

struct PipelineLayoutHolder {
    pub value: vk_sys::PipelineLayout
}

fn on_load(env: Env, _info: Term) -> bool {
    resource_struct_init!(InstanceHolder, env);
    resource_struct_init!(DeviceHolder, env);
    resource_struct_init!(QueueHolder, env);
    resource_struct_init!(CommandPoolHolder, env);
    resource_struct_init!(BufferHolder, env);
    resource_struct_init!(DeviceMemoryHolder, env);
    resource_struct_init!(DescriptorSetLayoutHolder, env);
    resource_struct_init!(DescriptorPoolHolder, env);
    resource_struct_init!(DescriptorSetHolder, env);
    resource_struct_init!(ShaderModuleHolder, env);
    resource_struct_init!(PipelineLayoutHolder, env);
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
        vk_sys::ERROR_DEVICE_LOST =>
            Ok(erl_error(env, atoms::device_lost())),
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
        let mut pdevices: Vec<vk_sys::PhysicalDevice> = vec![mem::uninitialized(); count as usize];
        r = vkEnumeratePhysicalDevices(inst_holder.value, &mut count, pdevices.as_mut_ptr());
        (r, pdevices)
    };

    match_return(env, result, devices)
}

fn get_physical_device_properties_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let physical_device: vk_sys::PhysicalDevice = args[0].decode()?;
    let props = unsafe {
        let mut p: vk_sys::PhysicalDeviceProperties = mem::uninitialized();
        vkGetPhysicalDeviceProperties(physical_device, &mut p);
        p
    };

    let dev_name: &str = unsafe {CStr::from_ptr(props.deviceName.as_ptr())}.to_str().unwrap();

    let erl_props = ErlVkPhysicalDeviceProperties {
        api_version: props.apiVersion,
        driver_version: props.driverVersion,
        vendor_id: props.vendorID,
        device_id: props.deviceID,
        device_type: props.deviceType,
        device_name: dev_name.to_owned(),
        pipeline_cache_uuid: props.pipelineCacheUUID.to_vec(),
        nothing1: None,
        nothing2: None
    };

    Ok(erl_props.encode(env))
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
        let erl_extent = ErlVkExtent3D {
            width: extent.width,
            height: extent.height,
            depth: extent.depth
        };
        let erl_property = ErlVkQueueFamilyProperties {
            queue_flags: prop.queueFlags,
            queue_count: prop.queueCount,
            timestamp_valid_bits: prop.timestampValidBits,
            min_image_transfer_granularity: erl_extent,
            family_index: idx as u32
        };
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

fn bind_buffer_memory_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let buffer: ResourceArc<BufferHolder> = args[1].decode()?;
    let memory: ResourceArc<DeviceMemoryHolder> = args[2].decode()?;
    let offset: vk_sys::DeviceSize = args[3].decode()?;

    let result = unsafe {
        vkBindBufferMemory(logi_device.value, buffer.value, memory.value, offset)
    };

    let ret = match result {
        vk_sys::SUCCESS =>
            atoms::ok(),
        vk_sys::ERROR_OUT_OF_HOST_MEMORY =>
            atoms::out_of_host_memory(),
        vk_sys::ERROR_OUT_OF_DEVICE_MEMORY =>
            atoms::out_of_device_memory(),
        _ =>
            atoms::nif_error()
    };

    Ok(ret.encode(env))
}

fn create_descriptor_set_layout_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_create_info: ErlVkDescriptorSetLayoutCreateInfo = args[1].decode()?;

    let mut set: Vec<vk_sys::DescriptorSetLayoutBinding> = Vec::new();
    let len = erl_create_info.bindings.len();

    for i in 0..len {
        let erl_binding = &erl_create_info.bindings[i];
        set.push(vk_sys::DescriptorSetLayoutBinding {
            binding: erl_binding.binding,
            descriptorType: erl_binding.descriptor_type,
            descriptorCount: erl_binding.descriptor_count,
            stageFlags: erl_binding.stage_flags,
            pImmutableSamplers: null()
        })
    }

    let vk_create_info = vk_sys::DescriptorSetLayoutCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        pNext: null(),
        flags: erl_create_info.flags,
        bindingCount: len as u32,
        pBindings: set.as_ptr()
    };

    let (result, set_layout) = unsafe {
        let mut sl: DescriptorSetLayoutHolder = mem::uninitialized();
        let r = vkCreateDescriptorSetLayout(logi_device.value, &vk_create_info, null(), &mut sl.value);
        (r, sl)
    };

    match_return(env, result, ResourceArc::new(set_layout))
}

fn destroy_descriptor_set_layout_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let set_layout: ResourceArc<DescriptorSetLayoutHolder> = args[1].decode()?;

    unsafe {
        vkDestroyDescriptorSetLayout(logi_device.value, set_layout.value, null());
    }

    Ok(atoms::ok().encode(env))
}

fn create_descriptor_pool_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_create_info: ErlVkDescriptorPoolCreateInfo = args[1].decode()?;
    let map_fun = |p:&ErlVkDescriptorPoolSize| vk_sys::DescriptorPoolSize{
        descriptorCount: p.descriptor_count,
        ty: p.descriptor_type
    };

    let vk_pool_sizes: Vec<vk_sys::DescriptorPoolSize> =
        erl_create_info.pool_sizes.iter().map(map_fun).collect();

    let vk_create_info = vk_sys::DescriptorPoolCreateInfo{
        sType: vk_sys::STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        pNext: null(),
        flags: erl_create_info.flags,
        maxSets: erl_create_info.max_sets,
        poolSizeCount: vk_pool_sizes.len() as u32,
        pPoolSizes: vk_pool_sizes.as_ptr()
    };

    let (result, pool) = unsafe {
        let mut p: DescriptorPoolHolder = mem::uninitialized();
        let r = vkCreateDescriptorPool(logi_device.value, &vk_create_info, null(), &mut p.value);
        (r, p)
    };

    match_return(env, result, ResourceArc::new(pool))
}

fn destroy_descriptor_pool_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let pool: ResourceArc<DescriptorPoolHolder> = args[1].decode()?;

    unsafe {
        vkDestroyDescriptorPool(logi_device.value, pool.value, null())
    };

    Ok(atoms::ok().encode(env))
}

fn allocate_descriptor_sets_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_alloc_info: ErlVkDescriptorSetAllocateInfo = args[1].decode()?;

    let map_fun = |l:&ResourceArc<DescriptorSetLayoutHolder>| l.value;

    let set_layouts: Vec<vk_sys::DescriptorSetLayout> =
        erl_alloc_info.set_layouts.iter().map(map_fun).collect();

    let vk_alloc_info = vk_sys::DescriptorSetAllocateInfo {
        sType: vk_sys::STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        pNext: null(),
        descriptorPool: erl_alloc_info.pool.value,
        descriptorSetCount: set_layouts.len() as u32,
        pSetLayouts: set_layouts.as_ptr()
    };

    let (result, sets): (vk_sys::Result, Vec<ResourceArc<DescriptorSetHolder>>) = unsafe {
        let mut ss: Vec<vk_sys::DescriptorSet> = Vec::new();
        ss.reserve(set_layouts.len());
        ss.set_len(set_layouts.len());
        let r = vkAllocateDescriptorSets(logi_device.value, &vk_alloc_info, ss.as_mut_ptr());
        let wrap_fun = |ds: &vk_sys::DescriptorSet| ResourceArc::new(DescriptorSetHolder{value: *ds});
        (r, ss.iter().map(wrap_fun).collect())
    };

    match_return(env, result, sets)
}

fn free_descriptor_sets_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let pool: ResourceArc<DescriptorPoolHolder> = args[1].decode()?;
    let erl_sets: Vec<ResourceArc<DescriptorSetHolder>> = args[2].decode()?;

    let unwrap_fun = |h:&ResourceArc<DescriptorSetHolder>|h.value;
    let vk_sets:Vec<vk_sys::DescriptorSet> = erl_sets.iter().map(unwrap_fun).collect();

    let result = unsafe {
        vkFreeDescriptorSets(logi_device.value, pool.value, vk_sets.len() as u32, vk_sets.as_ptr())
    };

    let term = match result {
        vk_sys::SUCCESS => atoms::ok().encode(env),
        vk_sys::ERROR_OUT_OF_HOST_MEMORY => erl_error(env, atoms::out_of_host_memory()),
        vk_sys::ERROR_OUT_OF_DEVICE_MEMORY => erl_error(env, atoms::out_of_device_memory()),
        _ => atoms::nif_error().encode(env)
    };

    Ok(term)
}

fn update_descriptor_sets_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_writes: Vec<ErlVkWriteDescriptorSet> = args[1].decode()?;
    let erl_copies: Vec<ErlVkCopyDescriptorSet> = args[2].decode()?;

    let map_vec_info = |x: &ErlVkDescriptorBufferInfo|vk_sys::DescriptorBufferInfo{
        range: x.range,
        offset: x.offset,
        buffer: x.buffer.value
    };
    let map_buffer_info = |x: &ErlVkWriteDescriptorSet|x.buffer_info.iter().map(map_vec_info).collect();

    let map_copies = |x: &ErlVkCopyDescriptorSet|vk_sys::CopyDescriptorSet{
        sType: vk_sys::STRUCTURE_TYPE_COPY_DESCRIPTOR_SET,
        pNext: null(),
        srcSet: x.src_set.value,
        srcBinding: x.src_binding,
        srcArrayElement: x.src_array_element,
        dstSet: x.dst_set.value,
        dstBinding: x.dst_binding,
        dstArrayElement: x.dst_array_element,
        descriptorCount: x.descriptor_count,
    };

    let vk_copies: Vec<vk_sys::CopyDescriptorSet> = erl_copies.iter().map(map_copies).collect();
    let buffers: Vec<Vec<vk_sys::DescriptorBufferInfo>> = erl_writes.iter().map(map_buffer_info).collect();
    let mut vk_writes: Vec<vk_sys::WriteDescriptorSet> = Vec::new();

    for (i, x) in erl_writes.iter().enumerate() {
        let ds = vk_sys::WriteDescriptorSet {
            sType: vk_sys::STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
            pNext: null(),
            dstSet: x.dst_set.value,
            dstBinding: x.dst_binding,
            dstArrayElement: x.dst_array_element,
            descriptorCount: buffers[i].len() as u32,
            descriptorType: x.descriptor_type,
            pImageInfo: null(),
            pBufferInfo: buffers[i].as_ptr(),
            pTexelBufferView: null()
        };
        vk_writes.push(ds);
    }

    unsafe {
        vkUpdateDescriptorSets(logi_device.value, vk_writes.len() as u32, vk_writes.as_ptr(), vk_copies.len() as u32, vk_copies.as_ptr());
    };

    Ok(atoms::ok().encode(env))
}

fn create_shader_module_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let mut erl_create_info: ErlVkShaderModuleCreateInfo = args[1].decode()?;

    let len:u32 = erl_create_info.code.len() as u32;

    let padded_len:u32 = (len as f32 / 4.0).ceil() as u32 * 4;

    if padded_len > len {
        erl_create_info.code.reserve_exact((padded_len - len) as usize);
    }

    let create_info = vk_sys::ShaderModuleCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        pNext: null(),
        flags: 0,
        codeSize: padded_len as usize,
        pCode: erl_create_info.code.as_ptr() as *const u32
    };

    let (result, shader) = unsafe {
        let mut s = ShaderModuleHolder { value : mem::uninitialized()} ;
        let r = vkCreateShaderModule(logi_device.value, &create_info, null(), &mut s.value);
        (r, s)
    };

    match_return(env, result, ResourceArc::new(shader))
}

fn destroy_shader_module_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let shader: ResourceArc<ShaderModuleHolder> = args[1].decode()?;

    unsafe { vkDestroyShaderModule(logi_device.value, shader.value, null())};

    Ok(atoms::ok().encode(env))
}

fn create_pipeline_layout_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let erl_create_info: ErlVkPipelineCreateInfo = args[1].decode()?;

    let map_set_layouts = | x: &ResourceArc<DescriptorSetLayoutHolder> | x.value;
    let set_layouts: Vec<vk_sys::DescriptorSetLayout> = erl_create_info.set_layouts.iter().map(map_set_layouts).collect();

    let map_const_ranges = |x: &ErlVkPushConstantRange|vk_sys::PushConstantRange{
        stageFlags: x.flags,
        offset: x.offset,
        size: x.size,
    };
    let constant_ranges: Vec<vk_sys::PushConstantRange> = erl_create_info.push_constant_ranges.iter().map(map_const_ranges).collect();

    let create_info = vk_sys::PipelineLayoutCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        pNext: null(),
        flags: 0,
        setLayoutCount: set_layouts.len() as u32,
        pSetLayouts: set_layouts.as_ptr(),
        pushConstantRangeCount: constant_ranges.len() as u32,
        pPushConstantRanges: constant_ranges.as_ptr(),
    };

    let (result, pipeline) = unsafe {
        let mut p = PipelineLayoutHolder {value: mem::uninitialized()};
        let r = vkCreatePipelineLayout(logi_device.value, &create_info, null(), &mut p.value);
        (r, p)
    };

    match_return(env, result, ResourceArc::new(pipeline))
}

fn destroy_pipeline_layout_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let logi_device: ResourceArc<DeviceHolder> = args[0].decode()?;
    let pipeline: ResourceArc<PipelineLayoutHolder> = args[1].decode()?;

    unsafe {
        vkDestroyPipelineLayout(logi_device.value, pipeline.value, null());
    }

    Ok(atoms::ok().encode(env))
}
