// converts C command-line args to nice Orange slices
// Copyright (c) 2023-2025, Joseph Shimel and contributors.

#ifndef ORANGE_CORE_ARGS_INC
#define ORANGE_CORE_ARGS_INC

#include <string.h>

static inline orange_type_ArgsCtx orange__core__convert_args(int argc, char **argv)
{
    orange_type_StringSlice result;

    result._0 = malloc(sizeof(orange_type_String) * (size_t)argc);
    result._1 = (int64_t)argc;

    for (int i = 0; i < argc; i += 1)
    {
        char *arg = argv[i];
        size_t len = strlen(arg);

        result._0[i]._0 = (uint8_t *)arg;
        result._0[i]._1 = (int64_t)len;
    }

    return (orange_type_ArgsCtx){._0 = result};
}

#endif
