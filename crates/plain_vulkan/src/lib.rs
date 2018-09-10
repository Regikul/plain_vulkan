extern crate vk_sys;
#[macro_use]
extern crate rustler;
#[macro_use]
extern crate lazy_static;

use rustler::{Env, Term, Error, Encoder};
use rustler::resource::ResourceArc;
use std::ffi::{CString, CStr, NulError};
use std::mem;

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

        // records
        atom vk_physical_device_properties;
        atom vk_extent_3d;
        atom vk_queue_family_properties;
    }
}

struct Holder<T> {
    value: T
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
    ],
    Some(on_load)
);

fn on_load(env: Env, _info: Term) -> bool {
    resource_struct_init!(Holder<vk_sys::Instance>, env);
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
fn match_return<'a, T: Encoder>(env: Env<'a>,result: vk_sys::Result, data: T) -> Result<Term<'a>, Error> {
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
        pNext: std::ptr::null(),
        pApplicationName: app_name.as_ptr(),
        applicationVersion: vk_make_version(0, 1,0),
        pEngineName: app_name.as_ptr(),
        engineVersion: vk_make_version(0, 1, 0),
        apiVersion: vk_make_version(1, 0, 0),
    };

    let create_info = vk_sys::InstanceCreateInfo {
        sType: vk_sys::STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        pNext: std::ptr::null(),
        flags: 0,
        pApplicationInfo: &app_info,
        enabledLayerCount: 0,
        ppEnabledLayerNames: std::ptr::null(),
        enabledExtensionCount: 0,
        ppEnabledExtensionNames: std::ptr::null(),
    };

    let (result, instance) = unsafe {
        let mut holder = Holder {
            value: mem::uninitialized()
        };
        let vk_result = vkCreateInstance(&create_info, std::ptr::null(), &mut holder.value);
        (vk_result, holder)
    };

    match_return(env, result, ResourceArc::new(instance))
}

fn destroy_instance_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder : ResourceArc<Holder<vk_sys::Instance>> = args[0].decode()?;

    unsafe {
        vkDestroyInstance(inst_holder.value, std::ptr::null());
    }

    Ok(atoms::ok().encode(env))
}

fn count_physical_devices_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder: ResourceArc<Holder<vk_sys::Instance>> = args[0].decode()?;

    let (result, count) = unsafe {
        let mut c:u32 = 0;
        let r: vk_sys::Result;
        r = vkEnumeratePhysicalDevices(inst_holder.value, &mut c, std::ptr::null_mut());
        (r, c)
    };

    match_return(env, result, count)
}

fn enumerate_physical_devices_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder: ResourceArc<Holder<vk_sys::Instance>> = args[0].decode()?;
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
        vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &mut c, std::ptr::null_mut());
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
