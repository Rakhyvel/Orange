// root allocator context implementation
// Copyright (c) 2023-2025, Joseph Shimel and contributors.

#ifndef ORANGE_CORE_FILE_IO_INC
#define ORANGE_CORE_FILE_IO_INC

#include <stdio.h>
#include <stdlib.h>

static inline void orange__core__close(void *self)
{
    if (NULL != self)
    {
        fclose((FILE *)self);
    }
}

const struct vtable_core__core__0_File orange__core__file_vtable = {
    .close = orange__core__close,
};

static inline struct orange_type_F3D431398B6D5D16 orange__core__open(void *self, struct orange_type_73877515D86B0F5 path, struct orange_type_C53265BF4F923210 mode)
{
    FILE *f;
    (void)self;

    switch (mode.tag)
    {
    case 0:
        f = fopen((const char *)path._0, "r");
        break;
    case 1:
        f = fopen((const char *)path._0, "w");
        break;
    case 2:
        f = fopen((const char *)path._0, "a");
        break;
    case 3:
        f = fopen((const char *)path._0, "rw");
        break;
    }

    if (NULL == f)
    {
        return (struct orange_type_F3D431398B6D5D16){.tag = 1};
    }
    else
    {
        return (struct orange_type_F3D431398B6D5D16){.tag = 0, ._0 = (struct orange_type_83D8D70C0300255B){.data_ptr = f, .vtable = &orange__core__file_vtable}};
    }
}

const struct vtable_core__core__0_File_System orange__core__file_system_vtable = {
    .open = orange__core__open,
};

#endif
