#ifndef ORNG_STD_MATH
#define ORNG_STD_MATH

#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

inline int64_t as_int_helper(double x)
{
    return (int64_t)x;
}

inline double as_float_helper(int64_t x)
{
    return (double)x;
}

inline double float_from_word32_helper(uint32_t x)
{
    return (double)x;
}

#endif
