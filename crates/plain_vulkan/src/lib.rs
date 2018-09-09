extern crate vk_sys;
#[macro_use]
extern crate rustler;
#[macro_use]
extern crate rustler_codegen;
#[macro_use]
extern crate lazy_static;

#[allow(unused_imports)]
use rustler::{Env, Term, Error, Encoder};
#[allow(unused_imports)]
use rustler::resource::ResourceArc;
use std::ffi::{CString, NulError};
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
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom nif_error;

        // Vulkan
        atom out_of_host_memory;
        atom out_of_device_memory;
        atom initialization_failed;
        atom layers_not_present;
        atom extension_not_present;
        atom incompatible_driver;
    }
}

struct InstanceHolder {
    instance: vk_sys::Instance
}

rustler_export_nifs!(
    "plain_vulkan",
    [("create_instance", 1, create_instance_nif)
    ,("destroy_instance", 1, destroy_instance_nif)
    ],
    Some(on_load)
);

fn on_load(env: Env, _info: Term) -> bool {
    resource_struct_init!(InstanceHolder, env);
    true
}

fn vk_make_version(major:u32, minor: u32, patch: u32) -> u32 {
    ((major) << 22) | ((minor) << 12) | (patch)
}

fn erl_ok<'a, T: Encoder>(env: Env<'a>, data: T) -> Term<'a> {
    (atoms::ok(), data).encode(env)
}

fn erl_error<'a, T: Encoder>(env: Env<'a>, data: T) -> Term<'a> {
    (atoms::error(), data).encode(env)
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
        let mut holder = InstanceHolder{
            instance: mem::uninitialized()
        };
        let vk_result = vkCreateInstance(&create_info, std::ptr::null(), &mut holder.instance);
        (vk_result, holder)
    };

    match result {
        vk_sys::SUCCESS => Ok(erl_ok(env, ResourceArc::new(instance).encode(env))),
        vk_sys::ERROR_OUT_OF_HOST_MEMORY => Ok(erl_error(env, atoms::out_of_host_memory())),
        vk_sys::ERROR_OUT_OF_DEVICE_MEMORY => Ok(erl_error(env, atoms::out_of_device_memory())),
        vk_sys::ERROR_INITIALIZATION_FAILED => Ok(erl_error(env, atoms::initialization_failed())),
        vk_sys::ERROR_LAYER_NOT_PRESENT => Ok(erl_error(env, atoms::layers_not_present())),
        vk_sys::ERROR_EXTENSION_NOT_PRESENT => Ok(erl_error(env, atoms::extension_not_present())),
        vk_sys::ERROR_INCOMPATIBLE_DRIVER => Ok(erl_error(env, atoms::incompatible_driver())),
        _ => Ok(erl_error(env, atoms::nif_error()))
    }

}

fn destroy_instance_nif<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let inst_holder : ResourceArc<InstanceHolder> = args[0].decode()?;

    unsafe {
        vkDestroyInstance(inst_holder.instance, std::ptr::null());
    }

    Ok(atoms::ok().encode(env))
}
