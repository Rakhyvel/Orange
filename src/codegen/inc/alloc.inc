// root allocator context implementation
// Copyright (c) 2023-2025, Joseph Shimel and contributors.

#ifndef ORANGE_CORE_ALLOC_INC
#define ORANGE_CORE_ALLOC_INC

static inline struct orange_type_F4C9DA480E915B59 orange__core__alloc(void *self, int64_t nbytes)
{
    void *retval = malloc((size_t)nbytes);
    (void)self;
    if (retval)
    {
        return (struct orange_type_F4C9DA480E915B59){.tag = 0, ._0 = retval};
    }
    else
    {
        return (struct orange_type_F4C9DA480E915B59){.tag = 1};
    }
}

static inline struct orange_type_F4C9DA480E915B59 orange__core__realloc(void *self, uint8_t *allocation, int64_t newsize)
{
    void *retval = realloc(allocation, (size_t)newsize);
    (void)self;
    if (retval)
    {
        return (struct orange_type_F4C9DA480E915B59){.tag = 0, ._0 = retval};
    }
    else
    {
        return (struct orange_type_F4C9DA480E915B59){.tag = 1};
    }
}

static inline void orange__core__free(void *self, uint8_t *allocation)
{
    (void)self;
    (void)free(allocation);
}

const struct vtable_core__core__0_Allocator orange__core__allocator_vtable = {
    ._alloc = orange__core__alloc,
    ._realloc = orange__core__realloc,
    ._free = orange__core__free,
};

#endif
