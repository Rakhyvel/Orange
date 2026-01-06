// root allocator context implementation
// Copyright (c) 2023-2026, Joseph Shimel and contributors.

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

static inline struct orange_type_C53265BF4F923210 orange__core__file_write(void *self, orange_type_String bytes)
{
    FILE *file = (FILE *)self;
    if (bytes._0 != NULL && bytes._1 > 0)
    {
        size_t written = fwrite(bytes._0, 1, (size_t)bytes._1, file);
        if (written < (size_t)bytes._1)
        {
            if (ferror(file))
            {
                clearerr(file);
                return (struct orange_type_C53265BF4F923210){.tag = 1};
            }
        }
        fflush(file);
    }
    return (struct orange_type_C53265BF4F923210){.tag = 0};
}

static inline struct orange_type_C53265BF4F923210 orange__core__file_read(void *self, struct orange_type_384D608F76C1F2D2 /*&mut dyn identifier("core")::Writer*/ writer, struct orange_type_CE26574889E9A595 maybe_limit)
{
    FILE *file = (FILE *)self;
    unsigned char buffer[4096];
    size_t total = 0;
    size_t limit;

    if (maybe_limit.tag == 0)
    {
        limit = (size_t)maybe_limit._0;
    }
    else
    {
        limit = SIZE_MAX;
    }

    while (1)
    {
        orange_type_String slice;
        struct orange_type_C53265BF4F923210 write_result;

        size_t n = fread(buffer, 1, MIN(sizeof(buffer), limit - total), file);
        if (n == 0)
        {
            if (ferror(file))
            {
                clearerr(file);
                return (struct orange_type_C53265BF4F923210){.tag = 1};
            }
            break;
        }
        total += n;
        slice._0 = buffer;
        slice._1 = (int64_t)n;

        write_result = writer.vtable->_write(writer.data_ptr, slice);
        if (write_result.tag == 1)
        {
            return write_result;
        }
    }

    return (struct orange_type_C53265BF4F923210){.tag = 0};
}

const struct vtable_core__core__0_Writer orange__core__file_writer_vtable = {
    ._write = orange__core__file_write,
};

const struct vtable_core__core__0_Reader orange__core__file_reader_vtable = {
    ._read = orange__core__file_read,
};

const struct vtable_core__core__0_File orange__core__file_vtable = {
    ._close = orange__core__close,
    ._0 = &orange__core__file_writer_vtable,
    ._1 = &orange__core__file_reader_vtable,
};

static inline struct orange_type_F3D431398B6D5D16 orange__core__open(void *self, orange_type_String path, struct orange_type_C53265BF4F923210 mode)
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
    ._open = orange__core__open,
};

#endif
