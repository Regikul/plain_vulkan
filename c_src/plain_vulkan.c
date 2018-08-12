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
#define ATOM_EXTEN_NOT_PRESENT      ATOM("extension_not_present")
#define ATOM_FEATURE_NOT_PRESENT    ATOM("feature_not_present")
#define ATOM_TOO_MANY_OBJECTS       ATOM("too_many_objects")
#define ATOM_DEVICE_LOST            ATOM("device_lost")
#define ATOM_NO_LAYER               ATOM("no_layer")

#define TUPLE_OK(Value)     enif_make_tuple(env, 2, ATOM_OK, Value)
#define TUPLE_ERROR(Value)  enif_make_tuple(env, 2, ATOM_ERROR, Value)

#define load_instance(Value) if (enif_get_resource(env, argv[0], vk_resources[VK_INSTANCE].resource_type, (void **)&Value) == 0) return enif_make_badarg(env);
#define load_device(Value) if (enif_get_resource(env, argv[0], vk_resources[VK_DEVICE].resource_type, (void **)&Value) == 0) return enif_make_badarg(env);
#define mk_erlang_bool(Value) (Value ? ATOM_TRUE : ATOM_FALSE)
#define from_erlang_bool(Value) (enif_is_identical(Value, ATOM_TRUE))

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

typedef enum {
    VK_INSTANCE,
    VK_LAYER_PROPS,
    VK_PHYS_DEV,
    VK_LOGI_DEV,
    VK_DEVICE,
    VK_QUEUE,
    VK_COMMAND_POOL,
    VK_BUFFER,
    VK_DEVICE_MEMORY,
    VK_DESCRIPTOR_SET_LAYOUT,
    VK_DESCRIPTOR_POOL,

    VK_RESOURCE_COUNT
} vk_resource_enumeration;

typedef struct {
    char *name;
    ErlNifResourceType *resource_type;
} vk_resource_definition;

vk_resource_definition vk_resources[] = {
    {"VK_INSTANCE", NULL},
    {"VK_LAYER_PROPS", NULL},
    {"VK_PHYS_DEV", NULL},
    {"VK_LOGI_DEV", NULL},
    {"VK_DEVICE", NULL},
    {"VK_QUEUE", NULL},
    {"VK_COMMAND_POOL", NULL},
    {"VK_BUFFER", NULL},
    {"VK_DEVICE_MEMORY", NULL},
    {"VK_DESCRIPTOR_SET_LAYOUT", NULL},
    {"VK_DESCRIPTOR_POOL", NULL}
};

static int open_resources(ErlNifEnv* env) {
    const char* mod = "plain_vulkan";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    int i = 0;

    for (i = 0; i < VK_RESOURCE_COUNT; i++) {
        const char* name = vk_resources[i].name;
        vk_resources[i].resource_type = enif_open_resource_type(env, mod, name, NULL, flags, NULL);
        if(vk_resources[i].resource_type == NULL)
            return -1;
    }

    return 0;
}

ENIF(create_instance_nif) {
    unsigned app_name_length = 0;
    VkInstance *instance;
    VkResult result;

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

    const char* validationlayers[] = { "VK_LAYER_LUNARG_parameter_validation",
                                       "VK_LAYER_LUNARG_object_tracker",
                                       "VK_LAYER_LUNARG_core_validation"
                                     };

    // initialize the VkInstanceCreateInfo structure
    VkInstanceCreateInfo inst_info = {};
    inst_info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    inst_info.pNext = NULL;
    inst_info.flags = 0;
    inst_info.pApplicationInfo = &app_info;
    inst_info.enabledExtensionCount = 0;
    inst_info.ppEnabledExtensionNames = NULL;
    inst_info.enabledLayerCount = 3;
    inst_info.ppEnabledLayerNames = validationlayers;

    instance = enif_alloc_resource(vk_resources[VK_INSTANCE].resource_type, sizeof(VkInstance));

    if (instance && ((result = vkCreateInstance(&inst_info, NULL, instance)) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, instance);
        enif_release_resource(instance);

        return TUPLE_OK(term);
    } else switch (result) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_INITIALIZATION_FAILED:
            return TUPLE_ERROR(ATOM_INIT_FAILED);
        case VK_ERROR_LAYER_NOT_PRESENT:
            return TUPLE_ERROR(ATOM_NO_LAYER);
        case VK_ERROR_EXTENSION_NOT_PRESENT:
            return TUPLE_ERROR(ATOM_EXTEN_NOT_PRESENT);
        case VK_ERROR_INCOMPATIBLE_DRIVER:
            return TUPLE_ERROR(ATOM("incompatible_driver"));
        default:
            return ATOM_NIF_ERROR;
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
            return TUPLE_ERROR(ATOM_NO_LAYER);
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

ENIF(get_physical_device_memory_properties_nif) {
    VkPhysicalDevice *device = NULL;
    VkPhysicalDeviceMemoryProperties memoryProperties;

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    vkGetPhysicalDeviceMemoryProperties(*device, &memoryProperties);

    ERL_NIF_TERM memory_types[memoryProperties.memoryTypeCount],
                 memory_heaps[memoryProperties.memoryHeapCount];

    for(unsigned i = 0; i < memoryProperties.memoryTypeCount; i++) {
        ERL_NIF_TERM propFlags,
                     heapIndex;

        propFlags = enif_make_uint(env, memoryProperties.memoryTypes[i].propertyFlags);
        heapIndex = enif_make_uint(env, memoryProperties.memoryTypes[i].heapIndex);
        memory_types[i] = enif_make_tuple(env, 2, propFlags, heapIndex);
    }

    for (unsigned i = 0; i < memoryProperties.memoryHeapCount; i++) {
        ERL_NIF_TERM device_size,
                     flags;

        device_size = enif_make_ulong(env, memoryProperties.memoryHeaps[i].size);
        flags = enif_make_uint(env, memoryProperties.memoryHeaps[i].flags);
        memory_heaps[i] = enif_make_tuple(env, 2, device_size, flags);
    }

    ERL_NIF_TERM types_list, heaps_list;

    types_list = enif_make_list_from_array(env, memory_types, memoryProperties.memoryTypeCount);
    heaps_list = enif_make_list_from_array(env, memory_heaps, memoryProperties.memoryHeapCount);

    return enif_make_tuple(env, 3, ATOM("vk_physical_device_memory_properties")
                                 , types_list
                                 , heaps_list
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
        res[i] = enif_make_tuple(env, 6, ATOM("vk_queue_family_properties")
                                       , enif_make_int(env, c->queueFlags)
                                       , enif_make_int(env, c->queueCount)
                                       , enif_make_int(env, c->timestampValidBits)
                                       , minImageTransferGranularity
                                       , enif_make_int(env, i)
                                       );
    }

    return TUPLE_OK(enif_make_list_from_array(env, res, count));
}

ENIF(create_device_nif) {
    VkPhysicalDevice *device = NULL;
    VkDevice *logic_dev = NULL;

    if (enif_get_resource(env, argv[0], vk_resources[VK_PHYS_DEV].resource_type, (void **)&device) == 0)
        return enif_make_badarg(env);

    int arity;
    const ERL_NIF_TERM *argv1;
    if (!enif_get_tuple(env, argv[1], &arity, &argv1) || !enif_is_identical(argv1[0], ATOM("vk_device_create_info")))
        return enif_make_badarg(env);

    ERL_NIF_TERM queue_info_list = argv1[1];

    if (!enif_is_list(env, queue_info_list))
        return enif_make_badarg(env);

    VkResult ret_code;
    uint32_t queue_info_count = 0;
    enif_get_list_length(env, queue_info_list, &queue_info_count);
    VkDeviceQueueCreateInfo queue_create_info[queue_info_count];

    for (unsigned i = 0; i < queue_info_count; i++) {
        const ERL_NIF_TERM *device_queue_create_info;
        ERL_NIF_TERM queue_info_list_head;
        enif_get_list_cell(env, queue_info_list, &queue_info_list_head, &queue_info_list);
        if (!enif_get_tuple(env, queue_info_list_head, &arity, &device_queue_create_info)
            || !enif_is_identical(device_queue_create_info[0], ATOM("vk_device_queue_create_info"))
           )
            return enif_make_badarg(env);


        queue_create_info[i].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queue_create_info[i].pNext = NULL;
        queue_create_info[i].flags = 0;
        if (!enif_get_int(env, device_queue_create_info[1], (int*)&queue_create_info[i].queueFamilyIndex))
            return enif_make_badarg(env);
        if (!enif_get_int(env, device_queue_create_info[2], (int*)&queue_create_info[i].queueCount))
            return enif_make_badarg(env);
        unsigned int priority_length;
        if (!enif_get_list_length(env, device_queue_create_info[3], &priority_length))
            return enif_make_badarg(env);

        float *priorities = enif_alloc(priority_length * sizeof(float));
        ERL_NIF_TERM tail = device_queue_create_info[3];
        for (unsigned j = 0; !enif_is_empty_list(env, tail) && j < priority_length; j++) {
            ERL_NIF_TERM head;
            enif_get_list_cell(env, tail, &head, &tail);
            double val;
            if (!enif_get_double(env, head, &val)) {
                enif_free((void *)priorities);
                for (unsigned z = 0; i > 1 && z < i - 1; z++){
                    enif_free((void*)queue_create_info[z].pQueuePriorities);
                }
                return enif_make_badarg(env);
            }
            priorities[j] = val;
        }
        queue_create_info[i].pQueuePriorities = priorities;
    }

    const ERL_NIF_TERM *features;

    if (!enif_get_tuple(env, argv1[2], &arity, &features) || arity != 56)
        return enif_make_badarg(env);

    VkPhysicalDeviceFeatures device_features = {0};

    device_features.robustBufferAccess = from_erlang_bool(features[1]);
    device_features.fullDrawIndexUint32 = from_erlang_bool(features[2]);
    device_features.imageCubeArray = from_erlang_bool(features[3]);
    device_features.independentBlend = from_erlang_bool(features[4]);
    device_features.geometryShader = from_erlang_bool(features[5]);
    device_features.tessellationShader = from_erlang_bool(features[6]);
    device_features.sampleRateShading = from_erlang_bool(features[7]);
    device_features.dualSrcBlend = from_erlang_bool(features[8]);
    device_features.logicOp = from_erlang_bool(features[9]);
    device_features.multiDrawIndirect = from_erlang_bool(features[10]);
    device_features.drawIndirectFirstInstance = from_erlang_bool(features[11]);
    device_features.depthClamp = from_erlang_bool(features[12]);
    device_features.depthBiasClamp = from_erlang_bool(features[13]);
    device_features.fillModeNonSolid = from_erlang_bool(features[14]);
    device_features.depthBounds = from_erlang_bool(features[15]);
    device_features.wideLines = from_erlang_bool(features[16]);
    device_features.largePoints = from_erlang_bool(features[17]);
    device_features.alphaToOne = from_erlang_bool(features[18]);
    device_features.multiViewport = from_erlang_bool(features[19]);
    device_features.samplerAnisotropy = from_erlang_bool(features[20]);
    device_features.textureCompressionETC2 = from_erlang_bool(features[21]);
    device_features.textureCompressionASTC_LDR = from_erlang_bool(features[22]);
    device_features.textureCompressionBC = from_erlang_bool(features[23]);
    device_features.occlusionQueryPrecise = from_erlang_bool(features[24]);
    device_features.pipelineStatisticsQuery = from_erlang_bool(features[25]);
    device_features.vertexPipelineStoresAndAtomics = from_erlang_bool(features[26]);
    device_features.fragmentStoresAndAtomics = from_erlang_bool(features[27]);
    device_features.shaderTessellationAndGeometryPointSize = from_erlang_bool(features[28]);
    device_features.shaderImageGatherExtended = from_erlang_bool(features[29]);
    device_features.shaderStorageImageExtendedFormats = from_erlang_bool(features[30]);
    device_features.shaderStorageImageMultisample = from_erlang_bool(features[31]);
    device_features.shaderStorageImageReadWithoutFormat = from_erlang_bool(features[32]);
    device_features.shaderStorageImageWriteWithoutFormat = from_erlang_bool(features[33]);
    device_features.shaderUniformBufferArrayDynamicIndexing = from_erlang_bool(features[34]);
    device_features.shaderSampledImageArrayDynamicIndexing = from_erlang_bool(features[35]);
    device_features.shaderStorageBufferArrayDynamicIndexing = from_erlang_bool(features[36]);
    device_features.shaderStorageImageArrayDynamicIndexing = from_erlang_bool(features[37]);
    device_features.shaderClipDistance = from_erlang_bool(features[38]);
    device_features.shaderCullDistance = from_erlang_bool(features[39]);
    device_features.shaderFloat64 = from_erlang_bool(features[40]);
    device_features.shaderInt64 = from_erlang_bool(features[41]);
    device_features.shaderInt16 = from_erlang_bool(features[42]);
    device_features.shaderResourceResidency = from_erlang_bool(features[43]);
    device_features.shaderResourceMinLod = from_erlang_bool(features[44]);
    device_features.sparseBinding = from_erlang_bool(features[45]);
    device_features.sparseResidencyBuffer = from_erlang_bool(features[46]);
    device_features.sparseResidencyImage2D = from_erlang_bool(features[47]);
    device_features.sparseResidencyImage3D = from_erlang_bool(features[48]);
    device_features.sparseResidency2Samples = from_erlang_bool(features[49]);
    device_features.sparseResidency4Samples = from_erlang_bool(features[50]);
    device_features.sparseResidency8Samples = from_erlang_bool(features[51]);
    device_features.sparseResidency16Samples = from_erlang_bool(features[52]);
    device_features.sparseResidencyAliased = from_erlang_bool(features[53]);
    device_features.variableMultisampleRate = from_erlang_bool(features[54]);
    device_features.inheritedQueries = from_erlang_bool(features[55]);

    VkDeviceCreateInfo create_info = {0};
    create_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    create_info.queueCreateInfoCount = queue_info_count;
    create_info.pQueueCreateInfos = queue_create_info;
    create_info.pEnabledFeatures = &device_features;

    logic_dev = enif_alloc_resource(vk_resources[VK_LOGI_DEV].resource_type, sizeof(VkDevice));

    if (logic_dev && (ret_code = vkCreateDevice(*device, &create_info, NULL, logic_dev) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, logic_dev);
        enif_release_resource(logic_dev);
        return TUPLE_OK(term);
    } else switch (ret_code) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_INITIALIZATION_FAILED:
            return TUPLE_ERROR(ATOM_INIT_FAILED);
        case VK_ERROR_EXTENSION_NOT_PRESENT:
            return TUPLE_ERROR(ATOM_EXTEN_NOT_PRESENT);
        case VK_ERROR_FEATURE_NOT_PRESENT:
            return TUPLE_ERROR(ATOM_FEATURE_NOT_PRESENT);
        case VK_ERROR_TOO_MANY_OBJECTS:
            return TUPLE_ERROR(ATOM_TOO_MANY_OBJECTS);
        case VK_ERROR_DEVICE_LOST:
            return TUPLE_ERROR(ATOM_DEVICE_LOST);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(device_wait_idle_nif) {
    VkDevice *logic_dev = NULL;

    if (enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&logic_dev) == 0)
        return enif_make_badarg(env);

    vkDeviceWaitIdle(*logic_dev);

    return ATOM_OK;
}

ENIF(destroy_device_nif) {
    VkDevice *logic_dev = NULL;

    if (enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&logic_dev) == 0)
        return enif_make_badarg(env);

    vkDestroyDevice(*logic_dev, NULL);

    return ATOM_OK;
}

ENIF(get_device_queue_nif) {
    VkDevice *device = NULL;
    uint32_t queueFamilyIndex, queueIndex;
    VkQueue *queue;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_uint(env, argv[1], &queueFamilyIndex))
        return enif_make_badarg(env);

    if (!enif_get_uint(env, argv[2], &queueIndex))
        return enif_make_badarg(env);

    queue = enif_alloc_resource(vk_resources[VK_QUEUE].resource_type, sizeof(VkQueue));
    vkGetDeviceQueue(*device, queueFamilyIndex, queueIndex, queue);

    ERL_NIF_TERM term = enif_make_resource(env, queue);
    enif_release_resource(queue);

    return term;
}

ENIF(create_command_pool_nif) {
    VkDevice *device = NULL;
    int arity = 0;
    const ERL_NIF_TERM *record;
    VkResult result;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_is_tuple(env, argv[1]))
        return enif_make_badarg(env);

    VkCommandPoolCreateInfo info;

    if (!enif_get_tuple(env, argv[1], &arity, &record) ||
        !enif_is_identical(record[0], ATOM("vk_command_pool_create_info"))){

        return enif_make_badarg(env);
    }

    info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    info.pNext = NULL;
    enif_get_uint(env, record[1], &info.flags);
    enif_get_uint(env, record[2], &info.queueFamilyIndex);

    VkCommandPool *pool = enif_alloc_resource(vk_resources[VK_COMMAND_POOL].resource_type, sizeof(VkCommandPool));

    if (pool && ((result = vkCreateCommandPool(*device, &info, NULL, pool)) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, pool);
        enif_release_resource(pool);
        return TUPLE_OK(term);
    } else switch (result) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(destroy_command_pool_nif) {
    VkDevice *device;
    VkCommandPool *pool;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[1], vk_resources[VK_COMMAND_POOL].resource_type, (void**)&pool))
        return enif_make_badarg(env);

    vkDestroyCommandPool(*device, *pool, NULL);

    return ATOM_OK;
}

ENIF(create_buffer_nif) {
    VkDevice *device;
    const ERL_NIF_TERM *record;
    int arity = 0;
    VkBufferCreateInfo create_info = {0};
    VkResult result;
    VkBuffer *buffer;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &arity, &record) ||
        !enif_is_identical(record[0], ATOM("vk_buffer_create_info"))){

        return enif_make_badarg(env);
    }

    create_info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    if (!enif_get_uint(env, record[1], &create_info.flags))
        return enif_make_badarg(env);
    if (!enif_get_uint64(env, record[2], &create_info.size))
        return enif_make_badarg(env);
    if (!enif_get_uint(env, record[3], &create_info.usage))
        return enif_make_badarg(env);
    if (!enif_get_uint(env, record[4], &create_info.sharingMode))
        return enif_make_badarg(env);
    if (!enif_get_list_length(env, record[5], &create_info.queueFamilyIndexCount))
        return enif_make_badarg(env);

    uint32_t queueFamilyIndices[create_info.queueFamilyIndexCount];
    ERL_NIF_TERM list = record[5];
    ERL_NIF_TERM head;
    for (unsigned i = 0; enif_get_list_cell(env, list, &head, &list); i++) {
        if (!enif_get_uint(env, head, queueFamilyIndices + i))
            return enif_make_badarg(env);
    }
    create_info.pQueueFamilyIndices = queueFamilyIndices;

    buffer = enif_alloc_resource(vk_resources[VK_BUFFER].resource_type, sizeof(VkBuffer));

    if (buffer && ((result = vkCreateBuffer(*device, &create_info, NULL, buffer)) ==  VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, buffer);
        enif_release_resource(buffer);
        return TUPLE_OK(term);
    } else switch (result) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(destroy_buffer_nif) {
    VkDevice *device;
    VkBuffer *buffer;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[1], vk_resources[VK_BUFFER].resource_type, (void **)&buffer))
        return enif_make_badarg(env);

    vkDestroyBuffer(*device, *buffer, NULL);

    return ATOM_OK;
}

ENIF(get_buffer_memory_requirements_nif) {
    VkDevice *device;
    VkBuffer *buffer;
    VkMemoryRequirements memReq;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[1], vk_resources[VK_BUFFER].resource_type, (void **)&buffer))
        return enif_make_badarg(env);

    vkGetBufferMemoryRequirements(*device, *buffer, &memReq);

    return enif_make_tuple(env, 4, ATOM("vk_memory_requirements")
                                 , enif_make_ulong(env, memReq.size)
                                 , enif_make_ulong(env, memReq.alignment)
                                 , enif_make_uint(env, memReq.memoryTypeBits)
                                 );

}

ENIF(allocate_memory_nif) {
    VkDevice *device;
    VkMemoryAllocateInfo allocate_info = {0};
    const ERL_NIF_TERM *record;
    int arity = 0;
    VkDeviceSize size;
    uint32_t memoryTypeIndex;
    VkResult ret_code;
    VkDeviceMemory *device_memory;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &arity, &record) ||
        !enif_is_identical(record[0], ATOM("vk_memory_allocate_info"))){

        return enif_make_badarg(env);
    }
    if (!enif_get_ulong(env, record[1], &size))
        return enif_make_badarg(env);
    if (!enif_get_uint(env, record[2], &memoryTypeIndex))
        return enif_make_badarg(env);
    allocate_info.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocate_info.allocationSize = size;
    allocate_info.memoryTypeIndex = memoryTypeIndex;

    device_memory = enif_alloc_resource(vk_resources[VK_DEVICE_MEMORY].resource_type, sizeof(VkDeviceMemory));

    if (device_memory && (ret_code = vkAllocateMemory(*device, &allocate_info, NULL, device_memory) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, device_memory);
        enif_release_resource(device_memory);
        return TUPLE_OK(term);
    } else switch (ret_code) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        case VK_ERROR_TOO_MANY_OBJECTS:
            return TUPLE_ERROR(ATOM_TOO_MANY_OBJECTS);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(free_memory_nif) {
    VkDevice *device;
    VkDeviceMemory *memory;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[1], vk_resources[VK_DEVICE_MEMORY].resource_type, (void **)&memory))
        return enif_make_badarg(env);

    vkFreeMemory(*device, *memory, NULL);

    return ATOM_OK;
}

ENIF(bind_buffer_memory_nif) {
    VkDevice *device;
    VkBuffer *buffer;
    VkDeviceMemory *memory;
    VkDeviceSize offset;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[1], vk_resources[VK_BUFFER].resource_type, (void **)&buffer))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[2], vk_resources[VK_DEVICE_MEMORY].resource_type, (void **)&memory))
        return enif_make_badarg(env);

    if (!enif_get_ulong(env, argv[3], &offset))
        return enif_make_badarg(env);

    switch (vkBindBufferMemory(*device, *buffer, *memory, offset)) {
        case VK_SUCCESS:
            return ATOM_OK;
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return ATOM_OUT_OF_HOST_MEM;
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return ATOM_OUT_OF_DEVICE_MEM;
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(create_descriptor_set_layout_nif) {
    VkDevice *device;
    VkDescriptorSetLayoutCreateInfo create_info = {0};
    const ERL_NIF_TERM *record = NULL;
    int arity = 0;
    unsigned flags = 0;
    unsigned len = 0;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &arity, &record) ||
        !enif_is_identical(record[0], ATOM("vk_descriptor_set_layout_create_info"))){

        return enif_make_badarg(env);
    }
    if (!enif_get_uint(env, record[1], &flags))
        return enif_make_badarg(env);

    ERL_NIF_TERM list = record[2];
    if (!enif_get_list_length(env, list, &len))
        return enif_make_badarg(env);

    VkDescriptorSetLayoutBinding bindings[len];

    for (unsigned i = 0; i < len; i++) {
        ERL_NIF_TERM head;
        const ERL_NIF_TERM *binds = NULL;
        int barity = 0;
        if (!enif_get_list_cell(env, list, &head, &list))
            break;
        VkDescriptorSetLayoutBinding *binding = bindings + i;

        if (!enif_get_tuple(env, head, &barity, &binds) ||
            !enif_is_identical(binds[0], ATOM("vk_descriptor_set_layout_binding"))){

            return enif_make_badarg(env);
        }

        enif_get_uint(env, binds[1], &binding->binding);
        enif_get_uint(env, binds[2], &binding->descriptorType);
        enif_get_uint(env, binds[3], &binding->descriptorCount);
        enif_get_uint(env, binds[4], &binding->stageFlags);
        binding->pImmutableSamplers = NULL;
    }

    create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    create_info.flags = flags;
    create_info.bindingCount = len;
    create_info.pBindings = bindings;

    VkDescriptorSetLayout *set_layout = NULL;
    VkResult ret_code = VK_ERROR_OUT_OF_HOST_MEMORY;

    set_layout = enif_alloc_resource(vk_resources[VK_DESCRIPTOR_SET_LAYOUT].resource_type, sizeof(VkDescriptorSetLayout));

    if (set_layout && (ret_code = vkCreateDescriptorSetLayout(*device, &create_info, NULL, set_layout) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, set_layout);
        enif_release_resource(set_layout);
        return TUPLE_OK(term);
    } else switch (ret_code) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(destroy_descriptor_set_layout_nif) {
    VkDevice *device;
    VkDescriptorSetLayout *set_layout;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[1], vk_resources[VK_DESCRIPTOR_SET_LAYOUT].resource_type, (void **)&set_layout))
        return enif_make_badarg(env);

    vkDestroyDescriptorSetLayout(*device, *set_layout, NULL);

    return ATOM_OK;
}

ENIF(create_descriptor_pool_nif) {
    VkDevice *device;
    VkResult vk_result = VK_ERROR_OUT_OF_HOST_MEMORY;
    int arity;
    uint len;
    const ERL_NIF_TERM* record;
    VkDescriptorPool *descriptor_pool = NULL;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &arity, &record) ||
        !enif_is_identical(record[0], ATOM("vk_descriptor_pool_create_info"))){

        return enif_make_badarg(env);
    }

    VkDescriptorPoolCreateInfo create_info = {0};
    ERL_NIF_TERM erl_pool_sizes = record[3];

    create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
    enif_get_uint(env, record[1], &create_info.flags);
    enif_get_uint(env, record[2], &create_info.maxSets);
    enif_get_list_length(env, erl_pool_sizes, &len);

    VkDescriptorPoolSize pool_sizes[len];

    for(unsigned i = 0; i < len; i++) {
        ERL_NIF_TERM cell;
        int barity;
        const ERL_NIF_TERM *erl_pool_size;
        if (!enif_get_list_cell(env, erl_pool_sizes, &cell, &erl_pool_sizes))
            break;

        if (!enif_get_tuple(env, cell, &barity, &erl_pool_size) ||
            !enif_is_identical(erl_pool_size[0], ATOM("vk_descriptor_pool_size"))){

            return enif_make_badarg(env);
        }
        enif_get_uint(env, erl_pool_size[1], &pool_sizes[i].type);
        enif_get_uint(env, erl_pool_size[2], &pool_sizes[i].descriptorCount);
    }

    create_info.poolSizeCount = len;
    create_info.pPoolSizes = pool_sizes;

    descriptor_pool = enif_alloc_resource(vk_resources[VK_DESCRIPTOR_POOL].resource_type, sizeof(VkDescriptorPool));

    if (descriptor_pool && (vk_result = vkCreateDescriptorPool(*device, &create_info, NULL, descriptor_pool) == VK_SUCCESS)) {
        ERL_NIF_TERM term = enif_make_resource(env, descriptor_pool);
        enif_release_resource(descriptor_pool);
        return TUPLE_OK(term);
    } else switch (vk_result) {
        case VK_ERROR_OUT_OF_HOST_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_HOST_MEM);
        case VK_ERROR_OUT_OF_DEVICE_MEMORY:
            return TUPLE_ERROR(ATOM_OUT_OF_DEVICE_MEM);
        default:
            return ATOM_NIF_ERROR;
    }
}

ENIF(destroy_descriptor_pool_nif) {
    VkDevice *device;
    VkDescriptorPool *pool;

    if (!enif_get_resource(env, argv[0], vk_resources[VK_LOGI_DEV].resource_type, (void **)&device))
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[1], vk_resources[VK_DESCRIPTOR_POOL].resource_type, (void **)&pool))
        return enif_make_badarg(env);

    vkDestroyDescriptorPool(*device, *pool, NULL);

    return ATOM_OK;
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
  {"get_physical_device_memory_properties_nif", 1, get_physical_device_memory_properties_nif},
  {"get_physical_device_queue_family_count", 1, get_physical_device_queue_family_count_nif},
  {"get_physical_device_queue_family_properties_nif", 2, get_physical_device_queue_family_properties_nif},
  {"create_device", 2, create_device_nif},
  {"device_wait_idle", 1, device_wait_idle_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"destroy_device", 1, destroy_device_nif},
  {"get_device_queue", 3, get_device_queue_nif},
  {"create_command_pool_nif", 2, create_command_pool_nif},
  {"destroy_command_pool", 2, destroy_command_pool_nif},
  {"create_buffer_nif", 2, create_buffer_nif},
  {"destroy_buffer", 2, destroy_buffer_nif},
  {"get_buffer_memory_requirements_nif", 2, get_buffer_memory_requirements_nif},
  {"allocate_memory", 2, allocate_memory_nif},
  {"free_memory", 2, free_memory_nif},
  {"bind_buffer_memory", 4, bind_buffer_memory_nif},
  {"create_descriptor_set_layout_nif", 2, create_descriptor_set_layout_nif},
  {"destroy_descriptor_set_layout", 2, destroy_descriptor_set_layout_nif},
  {"create_descriptor_pool_nif", 2, create_descriptor_pool_nif},
  {"destroy_descriptor_pool", 2, destroy_descriptor_pool_nif}
};

ERL_NIF_INIT(plain_vulkan, nif_funcs, &load, NULL, &upgrade, NULL);
