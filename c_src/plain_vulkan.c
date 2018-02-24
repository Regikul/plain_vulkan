#include <erl_nif.h>
#include <vulkan/vulkan.h>

#define APPNAME_MAX_LENGTH 128

#define ATOM_OK     enif_make_atom(env, "ok")
#define ATOM_ERROR  enif_make_atom(env, "error")
#define TUPLE_OK(Value)    enif_make_tuple(env, 2, ATOM_OK, Value)
#define TUPLE_ERROR(Value)  enif_make_tuple(env, 2, ATOM_ERROR, Value)

typedef enum {
    VK_INSTANCE,
    VK_RESOURCE_COUNT
} vk_resource_enumeration;

typedef struct {
    char *name;
    void (*destructor)(ErlNifEnv* env, void* obj);
    ErlNifResourceType *resource_type;
} vk_resource_definition;

vk_resource_definition vk_resources[] = {
    {"VK_INSTANCE", NULL, NULL}
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

static ERL_NIF_TERM hello_world_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "hello_world");
}

static ERL_NIF_TERM create_instance_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
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

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return open_resources(env);
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return open_resources(env);
}

static ERL_NIF_TERM enum_phy_devs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint32_t count = 0;

    VkInstance *instance;

    if (enif_get_resource(env, argv[0], vk_resources[VK_INSTANCE].resource_type, (void **)&instance) == 0)
        return enif_make_badarg(env);

    ERL_NIF_TERM ret;

    switch(vkEnumeratePhysicalDevices(*instance, &count, NULL)) {
        case VK_SUCCESS:
            ret = enif_make_ulong(env, count);
            return TUPLE_OK(ret);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            ret = enif_make_atom(env, "out_of_host_memory");
            return TUPLE_ERROR(ret);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            ret = enif_make_atom(env, "out_of_host_memory");
            return TUPLE_ERROR(ret);
        case VK_ERROR_INITIALIZATION_FAILED:
            ret = enif_make_atom(env, "init_failed");
            return TUPLE_ERROR(ret);
    }
    return ATOM_ERROR;
}

static ERL_NIF_TERM destroy_instance_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VkInstance *instance;

    if (enif_get_resource(env, argv[0], vk_resources[VK_INSTANCE].resource_type, (void **)&instance) == 0)
        return enif_make_badarg(env);

    vkDestroyInstance(*instance, NULL);
    return ATOM_OK;
}

static ErlNifFunc nif_funcs[] = {
  {"hello_world", 0, hello_world_nif},
  {"create_instance", 1, create_instance_nif},
  {"enum_phy_devs", 1, enum_phy_devs_nif},
  {"destroy_instance", 1, destroy_instance_nif}
};

ERL_NIF_INIT(plain_vulkan, nif_funcs, &load, NULL, &upgrade, NULL);
