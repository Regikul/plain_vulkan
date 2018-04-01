#include <erl_nif.h>
#include <vulkan/vulkan.h>

#define APPNAME_MAX_LENGTH 128

#define ATOM(Value)     enif_make_atom(env, Value)
#define ATOM_OK         ATOM("ok")
#define ATOM_ERROR      ATOM("error")
#define ATOM_NIF_ERROR  ATOM("nif_error")
#define ATOM_UNDEFINED  ATOM("undefined")
#define ATOM_TRUE       ATOM("true")
#define ATOM_FALSE      ATOM("false")

#define ATOM_OUT_OF_HOST_MEM        ATOM("out_of_host_memory")
#define ATOM_OUT_OF_DEVICE_MEM      ATOM("out_of_device_memory")
#define ATOM_INIT_FAILED            ATOM("init_failed")

#define TUPLE_OK(Value)     enif_make_tuple(env, 2, ATOM_OK, Value)
#define TUPLE_ERROR(Value)  enif_make_tuple(env, 2, ATOM_ERROR, Value)

#define load_instance(Value) if (enif_get_resource(env, argv[0], vk_resources[VK_INSTANCE].resource_type, (void **)&Value) == 0) return enif_make_badarg(env);
#define load_device(Value) if (enif_get_resource(env, argv[0], vk_resources[VK_DEVICE].resource_type, (void **)&Value) == 0) return enif_make_badarg(env);
#define mk_erlang_bool(Value) (Value ? ATOM_TRUE : ATOM_FALSE)

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

typedef enum {
    VK_INSTANCE,
    VK_LAYER_PROPS,
    VK_PHYS_DEV,
    VK_LOGI_DEV,
    VK_DEVICE,
    VK_QUEUE,

    VK_RESOURCE_COUNT
} vk_resource_enumeration;

typedef struct {
    char *name;
    void (*destructor)(ErlNifEnv* env, void* obj);
    ErlNifResourceType *resource_type;
} vk_resource_definition;

vk_resource_definition vk_resources[] = {
    {"VK_INSTANCE", NULL, NULL},
    {"VK_LAYER_PROPS", NULL, NULL},
    {"VK_PHYS_DEV", NULL, NULL},
    {"VK_LOGI_DEV", NULL, NULL},
    {"VK_DEVICE", NULL, NULL},
    {"VK_QUEUE", NULL, NULL}
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

ENIF(count_instance_layer_properties_nif) {
    uint32_t count = 0;
    ERL_NIF_TERM ret;

    switch(vkEnumerateInstanceLayerProperties(&count, NULL)) {
        case VK_SUCCESS:
            ret = enif_make_ulong(env, count);
            return TUPLE_OK(ret);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(count_instance_extension_properties_nif) {
    uint32_t count = 0;
    ERL_NIF_TERM ret;
    unsigned layer_name_length = 0;

    enif_get_list_length(env, argv[0], &layer_name_length);

    char layer_name[layer_name_length + 1];
    if (enif_get_string(env, argv[0], layer_name, layer_name_length, ERL_NIF_LATIN1) == 0)
        return enif_make_badarg(env);

    switch(vkEnumerateInstanceExtensionProperties(layer_name, &count, NULL)) {
        case VK_SUCCESS:
            ret = enif_make_ulong(env, count);
            return TUPLE_OK(ret);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_LAYER_NOT_PRESENT:
            return TUPLE_ERROR(ATOM("no_layer"));
        default:
            return ATOM_NIF_ERROR;
    }
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
        default:
            return ATOM_NIF_ERROR;
    }
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
                enif_release_resource(allocated);
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
        default:
            return ATOM_NIF_ERROR;
    }
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
    VkPhysicalDevice *device = NULL;
    VkPhysicalDeviceFeatures features = {0};

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    vkGetPhysicalDeviceFeatures(*device, &features);

    return enif_make_tuple(env, 56, ATOM("vk_physical_device_features")
                                  , mk_erlang_bool(features.robustBufferAccess)
                                  , mk_erlang_bool(features.fullDrawIndexUint32)
                                  , mk_erlang_bool(features.imageCubeArray)
                                  , mk_erlang_bool(features.independentBlend)
                                  , mk_erlang_bool(features.geometryShader)
                                  , mk_erlang_bool(features.tessellationShader)
                                  , mk_erlang_bool(features.sampleRateShading)
                                  , mk_erlang_bool(features.dualSrcBlend)
                                  , mk_erlang_bool(features.logicOp)
                                  , mk_erlang_bool(features.multiDrawIndirect)
                                  , mk_erlang_bool(features.drawIndirectFirstInstance)
                                  , mk_erlang_bool(features.depthClamp)
                                  , mk_erlang_bool(features.depthBiasClamp)
                                  , mk_erlang_bool(features.fillModeNonSolid)
                                  , mk_erlang_bool(features.depthBounds)
                                  , mk_erlang_bool(features.wideLines)
                                  , mk_erlang_bool(features.largePoints)
                                  , mk_erlang_bool(features.alphaToOne)
                                  , mk_erlang_bool(features.multiViewport)
                                  , mk_erlang_bool(features.samplerAnisotropy)
                                  , mk_erlang_bool(features.textureCompressionETC2)
                                  , mk_erlang_bool(features.textureCompressionASTC_LDR)
                                  , mk_erlang_bool(features.textureCompressionBC)
                                  , mk_erlang_bool(features.occlusionQueryPrecise)
                                  , mk_erlang_bool(features.pipelineStatisticsQuery)
                                  , mk_erlang_bool(features.vertexPipelineStoresAndAtomics)
                                  , mk_erlang_bool(features.fragmentStoresAndAtomics)
                                  , mk_erlang_bool(features.shaderTessellationAndGeometryPointSize)
                                  , mk_erlang_bool(features.shaderImageGatherExtended)
                                  , mk_erlang_bool(features.shaderStorageImageExtendedFormats)
                                  , mk_erlang_bool(features.shaderStorageImageMultisample)
                                  , mk_erlang_bool(features.shaderStorageImageReadWithoutFormat)
                                  , mk_erlang_bool(features.shaderStorageImageWriteWithoutFormat)
                                  , mk_erlang_bool(features.shaderUniformBufferArrayDynamicIndexing)
                                  , mk_erlang_bool(features.shaderSampledImageArrayDynamicIndexing)
                                  , mk_erlang_bool(features.shaderStorageBufferArrayDynamicIndexing)
                                  , mk_erlang_bool(features.shaderStorageImageArrayDynamicIndexing)
                                  , mk_erlang_bool(features.shaderClipDistance)
                                  , mk_erlang_bool(features.shaderCullDistance)
                                  , mk_erlang_bool(features.shaderFloat64)
                                  , mk_erlang_bool(features.shaderInt64)
                                  , mk_erlang_bool(features.shaderInt16)
                                  , mk_erlang_bool(features.shaderResourceResidency)
                                  , mk_erlang_bool(features.shaderResourceMinLod)
                                  , mk_erlang_bool(features.sparseBinding)
                                  , mk_erlang_bool(features.sparseResidencyBuffer)
                                  , mk_erlang_bool(features.sparseResidencyImage2D)
                                  , mk_erlang_bool(features.sparseResidencyImage3D)
                                  , mk_erlang_bool(features.sparseResidency2Samples)
                                  , mk_erlang_bool(features.sparseResidency4Samples)
                                  , mk_erlang_bool(features.sparseResidency8Samples)
                                  , mk_erlang_bool(features.sparseResidency16Samples)
                                  , mk_erlang_bool(features.sparseResidencyAliased)
                                  , mk_erlang_bool(features.variableMultisampleRate)
                                  , mk_erlang_bool(features.inheritedQueries)
                                  );
}

ENIF(get_physical_device_queue_family_count_nif) {
    VkPhysicalDevice *device = NULL;
    uint32_t propCount = 0;

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    vkGetPhysicalDeviceQueueFamilyProperties(*device, &propCount, NULL);

    return TUPLE_OK(enif_make_int(env, propCount));
}

ENIF(get_physical_device_queue_family_properties_nif) {
    VkPhysicalDevice *device = NULL;
    uint32_t count = 0;

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    if (enif_get_ulong(env, argv[1], (unsigned long*)&count) == 0)
        return enif_make_badarg(env);

    VkQueueFamilyProperties props[count];
    ERL_NIF_TERM res[count];

    vkGetPhysicalDeviceQueueFamilyProperties(*device, &count, props);

    for(unsigned i = 0; i < count; i++) {
        VkQueueFamilyProperties *c = props + i;
        VkExtent3D *ce = &(c->minImageTransferGranularity);

        ERL_NIF_TERM minImageTransferGranularity = enif_make_tuple(env, 4, ATOM("vk_extent_3d")
                                                                         , enif_make_int(env, ce->width)
                                                                         , enif_make_int(env, ce->height)
                                                                         , enif_make_int(env, ce->depth)
                                                                         );
        ERL_NIF_TERM flags = enif_make_list(env, 0);
        if (c->queueFlags & VK_QUEUE_GRAPHICS_BIT)
            flags = enif_make_list_cell(env, ATOM("graphics"), flags);
        if (c->queueFlags & VK_QUEUE_COMPUTE_BIT)
            flags = enif_make_list_cell(env, ATOM("compute"), flags);
        if (c->queueFlags & VK_QUEUE_TRANSFER_BIT)
            flags = enif_make_list_cell(env, ATOM("transfer"), flags);
        if (c->queueFlags & VK_QUEUE_SPARSE_BINDING_BIT)
            flags = enif_make_list_cell(env, ATOM("sparse_bindigng"), flags);

        res[i] = enif_make_tuple(env, 5, ATOM("vk_queue_family_properties")
                                       , flags
                                       , enif_make_int(env, c->queueCount)
                                       , enif_make_int(env, c->timestampValidBits)
                                       , minImageTransferGranularity
                                       );
    }

    return TUPLE_OK(enif_make_list_from_array(env, res, count));
}

ENIF(create_device_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(destroy_device_nif) {
    return enif_make_tuple(env, 2, ATOM_ERROR, ATOM("not_implemented"));
}

ENIF(get_device_queue_nif) {
    VkDevice *device = NULL;
    uint32_t queueFamilyIndex, queueIndex;
    VkQueue *queue;

    load_device(device);

    queue = enif_alloc_resource(vk_resources[VK_QUEUE].resource_type, sizeof(VkQueue));
    vkGetDeviceQueue(*device, queueFamilyIndex, queueIndex, queue);

    ERL_NIF_TERM term = enif_make_resource(env, queue);
    enif_release_resource(queue);

    return TUPLE_OK(term);
}

ENIF(create_command_pool_nif) {
    int arity = 0;
    ERL_NIF_TERM *record;

    if (!enif_is_tuple(env, argv[1]))
        return enif_make_badarg(env);

    VkCommandPoolCreateInfo info;

    enif_get_tuple(env, argv[1], &arity, (const ERL_NIF_TERM**) &record);
    if (arity != 3)
        return enif_make_badarg(env);

    info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    info.pNext = NULL;
    enif_get_ulong(env, record[1], (unsigned long *) &info.queueFamilyIndex);
    enif_get_ulong(env, record[2], (unsigned long *) &info.flags);

    VkDevice *device = NULL;
    VkCommandPool *pool = NULL;

    load_device(device);

    ERL_NIF_TERM term;
    switch (vkCreateCommandPool(*device, &info, NULL, pool)) {
        case VK_SUCCESS:
            term = enif_make_resource(env, pool);
            enif_release_resource(pool);
            return TUPLE_OK(term);
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

static ErlNifFunc nif_funcs[] = {
  {"create_instance", 1, create_instance_nif},
  {"destroy_instance", 1, destroy_instance_nif},
  {"count_instance_layer_properties", 0, count_instance_layer_properties_nif},
  {"count_instance_extension_properties", 1, count_instance_extension_properties_nif},
  {"enumerate_instance_extension_properties", 2, enumerate_instance_extension_properties_nif},
  {"count_physical_devices", 1, count_physical_devices_nif},
  {"enumerate_physical_devices", 2, enumerate_physical_devices_nif},
  {"get_physical_device_properties", 1, get_physical_device_properties_nif},
  {"get_physical_device_features", 1, get_physical_device_features_nif},
  {"get_physical_device_queue_family_count", 1, get_physical_device_queue_family_count_nif},
  {"get_physical_device_queue_family_properties", 2, get_physical_device_queue_family_properties_nif},
  {"create_device", 2, create_device_nif},
  {"destroy_device", 1, destroy_device_nif},
  {"get_device_queue", 3, get_device_queue_nif},
  {"create_command_pool", 2, create_command_pool_nif}
};

ERL_NIF_INIT(plain_vulkan, nif_funcs, &load, NULL, &upgrade, NULL);
