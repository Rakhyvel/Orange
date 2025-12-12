// console input/output implementation
// Copyright (c) 2023-2025, Joseph Shimel and contributors.

#ifndef ORANGE_CORE_IO_INC
#define ORANGE_CORE_IO_INC

#include <stdio.h>

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

static inline struct orange_type_C53265BF4F923210 orange__core__write(void *self, orange_type_String bytes)
{
    (void)self;

    if (bytes._0 != NULL && bytes._1 > 0)
    {
        size_t written = fwrite(bytes._0, 1, (size_t)bytes._1, stdout);
        if (written < (size_t)bytes._1)
        {
            if (ferror(stdout))
            {
                clearerr(stdout);
                return (struct orange_type_C53265BF4F923210){.tag = 1};
            }
        }
        fflush(stdout);
    }
    return (struct orange_type_C53265BF4F923210){.tag = 0};
}

static inline struct orange_type_C53265BF4F923210 orange__core__read(void *self, struct orange_type_384D608F76C1F2D2 /*&mut dyn identifier("core")::Writer*/ writer, struct orange_type_CE26574889E9A595 maybe_limit)
{
    unsigned char buffer[4096];
    size_t total = 0;
    size_t limit;

    (void)self;

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

        size_t n = fread(buffer, 1, MIN(sizeof(buffer), limit - total), stdin);
        if (n == 0)
        {
            if (ferror(stdin))
            {
                clearerr(stdin);
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

const struct vtable_core__core__0_Writer orange__core__writer_vtable = {
    ._write = orange__core__write,
};

const struct vtable_core__core__0_Reader orange__core__reader_vtable = {
    ._read = orange__core__read,
};

#endif
