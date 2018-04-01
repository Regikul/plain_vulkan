#include <erl_nif.h>
#include <vulkan/vulkan.h>

#define APPNAME_MAX_LENGTH 128

#define ATOM(Value)     enif_make_atom(env, Value)
#define ATOM_OK         ATOM("ok")
#define ATOM_ERROR      ATOM("error")
#define ATOM_NIF_ERROR  ATOM("nif_error")
#define ATOM_UNDEFINED  ATOM("undefined")

#define ATOM_OUT_OF_HOST_MEM        ATOM("out_of_host_memory")
#define ATOM_OUT_OF_DEVICE_MEM      ATOM("out_of_device_memory")
#define ATOM_INIT_FAILED            ATOM("init_failed")

#define TUPLE_OK(Value)     enif_make_tuple(env, 2, ATOM_OK, Value)
#define TUPLE_ERROR(Value)  enif_make_tuple(env, 2, ATOM_ERROR, Value)

#define load_instance(Value) if (enif_get_resource(env, argv[0], vk_resources[VK_INSTANCE].resource_type, (void **)&Value) == 0) return enif_make_badarg(env);

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

typedef enum {
    VK_INSTANCE,
    VK_PHYS_DEV,
    VK_LOGI_DEV,

    VK_RESOURCE_COUNT
} vk_resource_enumeration;

typedef struct {
    char *name;
    void (*destructor)(ErlNifEnv* env, void* obj);
    ErlNifResourceType *resource_type;
} vk_resource_definition;

vk_resource_definition vk_resources[] = {
    {"VK_INSTANCE", NULL, NULL},
    {"VK_PHYS_DEV", NULL, NULL},
    {"VK_LOGI_DEV", NULL, NULL}
};

static int open_resources(ErlNifEnv* env) {
    const char* mod = "plain_vulkan";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    int i = 0;

    for (i = 0; i < VK_RESOURCE_COUNT; i++) {
        const char* name = vk_resources[i].name;
        void (*free_res) = vk_resources[i].destructor;
        vk_resources[i].resource_type = enif_open_resource_type(env, mod, name, free_res, flags, NULL);
        if(vk_resources[i].resource_type == NULL)
            return -1;
    }

    return 0;
}

ENIF(create_instance_nif) {
    unsigned app_name_length = 0;
    VkInstance *instance;

    enif_get_list_length(env, argv[0], &app_name_length);

    char app_name[app_name_length+1];

    if (enif_get_string(env, argv[0], app_name, app_name_length, ERL_NIF_LATIN1) == 0)
        return enif_make_badarg(env);

    // initialize the VkApplicationInfo structure
    VkApplicationInfo app_info = {};
    app_info.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pNext = NULL;
    app_info.pApplicationName = app_name;
    app_info.applicationVersion = 1;
    app_info.pEngineName = "erl_vulkan";
    app_info.engineVersion = 1;
    app_info.apiVersion = VK_API_VERSION_1_0;

    // initialize the VkInstanceCreateInfo structure
    VkInstanceCreateInfo inst_info = {};
    inst_info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    inst_info.pNext = NULL;
    inst_info.flags = 0;
    inst_info.pApplicationInfo = &app_info;
    inst_info.enabledExtensionCount = 0;
    inst_info.ppEnabledExtensionNames = NULL;
    inst_info.enabledLayerCount = 0;
    inst_info.ppEnabledLayerNames = NULL;

    instance = enif_alloc_resource(vk_resources[VK_INSTANCE].resource_type, sizeof(VkInstance));

    if (instance && (vkCreateInstance(&inst_info, NULL, instance) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, instance);
        enif_release_resource(instance);

        return TUPLE_OK(term);
    } else {
        return ATOM_ERROR;
    }
}

ENIF(destroy_instance_nif) {
    VkInstance *instance;

    load_instance(instance);

    vkDestroyInstance(*instance, NULL);
    return ATOM_OK;
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return open_resources(env);
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return open_resources(env);
}

ENIF(count_instance_extension_properties_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(enumerate_instance_extension_properties_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(count_physical_devices_nif) {
    uint32_t count = 0;
    VkInstance *instance;
    ERL_NIF_TERM ret;

    load_instance(instance);

    switch(vkEnumeratePhysicalDevices(*instance, &count, NULL)) {
        case VK_SUCCESS:
            ret = enif_make_ulong(env, count);
            return TUPLE_OK(ret);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_INITIALIZATION_FAILED:
            return TUPLE_ERROR(ATOM_INIT_FAILED);
    }
    return ATOM_NIF_ERROR;
}

ENIF(enumerate_physical_devices_nif) {
    VkInstance *instance;
    uint32_t count = 0;
    enif_get_ulong(env, argv[1], (unsigned long*)&count);
    VkPhysicalDevice devs[count];
    ERL_NIF_TERM res[count];
    VkPhysicalDevice *allocated;
    int i = 0;

    load_instance(instance);
    VkResult result;

    switch (result = vkEnumeratePhysicalDevices(*instance, &count, devs)) {
        case VK_SUCCESS:
        case VK_INCOMPLETE:
            for (; i < count; i++) {
                allocated  = (VkPhysicalDevice*) enif_alloc_resource(vk_resources[VK_PHYS_DEV].resource_type, sizeof(VkPhysicalDevice));
                *allocated = devs[i];
                res[i] = enif_make_resource(env, allocated);
                enif_release_resource(res[i]);
            }
            ERL_NIF_TERM ret = enif_make_list_from_array(env, res, count);
            if (result == VK_SUCCESS)
                return TUPLE_OK(ret);
            else
                return enif_make_tuple(env, 2, ATOM("incomplete"), ret);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_INITIALIZATION_FAILED:
            return TUPLE_ERROR(ATOM_INIT_FAILED);
    }
    return ATOM_NIF_ERROR;
}

ENIF(get_physical_device_properties_nif) {
    VkPhysicalDevice *device = NULL;
    VkPhysicalDeviceProperties properties = {0};
    ErlNifBinary uuid = {0};

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    vkGetPhysicalDeviceProperties(*device, &properties);

    if (enif_alloc_binary(VK_UUID_SIZE, &uuid) == 0)
        return ATOM_OUT_OF_HOST_MEM;

    return enif_make_tuple(env
                          ,10
                          ,ATOM("vk_physical_device_properties")
                          ,enif_make_int(env, properties.apiVersion)
                          ,enif_make_int(env, properties.driverVersion)
                          ,enif_make_int(env, properties.vendorID)
                          ,enif_make_int(env, properties.deviceID)
                          ,enif_make_int(env, properties.deviceType)
                          ,enif_make_string(env, properties.deviceName, ERL_NIF_LATIN1)
                          ,enif_make_binary(env, &uuid)
                          ,ATOM_UNDEFINED
                          ,ATOM_UNDEFINED
                          );
}

ENIF(get_physical_device_features_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(get_physical_device_queue_family_count_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(get_physical_device_queue_family_properties_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(create_device_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(destroy_device_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

static ErlNifFunc nif_funcs[] = {
  {"create_instance", 1, create_instance_nif},
  {"destroy_instance", 1, destroy_instance_nif},
  {"count_instance_extension_properties", 1, count_instance_extension_properties_nif},
  {"enumerate_instance_extension_properties", 2, enumerate_instance_extension_properties_nif},
  {"count_physical_devices", 1, count_physical_devices_nif},
  {"enumerate_physical_devices", 2, enumerate_physical_devices_nif},
  {"get_physical_device_properties", 1, get_physical_device_properties_nif},
  {"get_physical_device_features", 1, get_physical_device_features_nif},
  {"get_physical_device_queue_family_count", 1, get_physical_device_queue_family_count_nif},
  {"get_physical_device_queue_family_properties", 2, get_physical_device_queue_family_properties_nif},
  {"create_device", 2, create_device_nif},
  {"destroy_device", 1, destroy_device_nif}
};

ERL_NIF_INIT(plain_vulkan, nif_funcs, &load, NULL, &upgrade, NULL);
