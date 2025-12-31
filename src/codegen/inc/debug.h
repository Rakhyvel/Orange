// debug utilities made use by the Orange compiler when in debug mode
// Copyright (c) 2023-2026, Joseph Shimel and contributors.

#ifndef ORANGE_CORE_DEBUG_INC
#define ORANGE_CORE_DEBUG_INC

#include <stdnoreturn.h>
#include <stdio.h>
#include <stdint.h>

extern const char *orange_debug__lines[1024];
// pointer to current line in stack trace
extern uint16_t orange_debug__line_idx;
// pointer to top of stacktrace, where an error might be returned from
extern uint16_t orange_debug__err_line_idx;

inline static void orange_debug__dump_error_trace(const char *restrict msg)
{
    fprintf(stderr, "error: %s\n", msg);

    for (uint16_t orange_debug__i = 0; orange_debug__i < orange_debug__err_line_idx; orange_debug__i++)
    {
        fprintf(stderr, "%s\n", orange_debug__lines[orange_debug__err_line_idx - orange_debug__i - 1]);
    }
}

_Noreturn inline static void orange_debug__panic(const char *restrict msg)
{
    fprintf(stderr, "panic: %s\n", msg);
    for (uint16_t orange_debug__i = 0; orange_debug__i < orange_debug__line_idx; orange_debug__i++)
    {
        fprintf(stderr, "%s\n", orange_debug__lines[orange_debug__line_idx - orange_debug__i - 1]);
    }
    exit(1);
}

inline static void orange_debug__bounds_check(const int64_t idx, const int64_t length, const char *restrict line)
{
    if (0 > idx || idx >= length)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        orange_debug__panic("bounds check failed");
    }
}

inline static void orange_debug__tag_check(const uint64_t tag, const uint64_t sel, const char *restrict line)
{
    if (tag != sel)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        orange_debug__panic("inactive field");
    }
}

inline static int8_t orange_debug__negate_int8_t(const int8_t x, const char *restrict line)
{
    if (x == INT8_MIN)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "-(%d)", x);
        orange_debug__panic("Int8 negation overflow");
    }
    return -x;
}

inline static int16_t orange_debug__negate_int16_t(const int16_t x, const char *restrict line)
{
    if (x == INT16_MIN)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "-(%d)", x);
        orange_debug__panic("Int16 negation overflow");
    }
    return -x;
}

inline static int32_t orange_debug__negate_int32_t(const int32_t x, const char *restrict line)
{
    if (x == INT32_MIN)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "-(%d)", x);
        orange_debug__panic("Int32 negation overflow");
    }
    return -x;
}

inline static int64_t orange_debug__negate_int64_t(const int64_t x, const char *restrict line)
{
    if (x == INT64_MIN)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "-(%ld)\n", x);
        orange_debug__panic("Int64 negation overflow");
    }
    return -x;
}

inline static int8_t orange_debug__add_int8_t(const int8_t lhs, const int8_t rhs, const char *restrict line)
{
    if (((rhs > 0) && (lhs > (INT8_MAX - rhs))) ||
        ((rhs < 0) && (lhs < (INT8_MIN - rhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d + %d\n", lhs, rhs);
        orange_debug__panic("addition overflow");
    }
    return lhs + rhs;
}

inline static int16_t orange_debug__add_int16_t(const int16_t lhs, const int16_t rhs, const char *restrict line)
{
    if (((rhs > 0) && (lhs > (INT16_MAX - rhs))) ||
        ((rhs < 0) && (lhs < (INT16_MIN - rhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d + %d\n", lhs, rhs);
        orange_debug__panic("addition overflow");
    }
    return lhs + rhs;
}

inline static int32_t orange_debug__add_int32_t(const int32_t lhs, const int32_t rhs, const char *restrict line)
{
    if (((rhs > 0) && (lhs > (INT32_MAX - rhs))) ||
        ((rhs < 0) && (lhs < (INT32_MIN - rhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d + %d\n", lhs, rhs);
        orange_debug__panic("addition overflow");
    }
    return lhs + rhs;
}

inline static int64_t orange_debug__add_int64_t(const int64_t lhs, const int64_t rhs, const char *restrict line)
{
    if (((rhs > 0) && (lhs > (INT64_MAX - rhs))) ||
        ((rhs < 0) && (lhs < (INT64_MIN - rhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld + %ld\n", lhs, rhs);
        orange_debug__panic("addition overflow");
    }
    return lhs + rhs;
}

inline static int8_t orange_debug__sub_int8_t(const int8_t lhs, const int8_t rhs, const char *restrict line)
{
    if ((rhs > 0 && lhs < INT8_MIN + rhs) ||
        (rhs < 0 && lhs > INT8_MAX + rhs))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d - %d\n", lhs, rhs);
        orange_debug__panic("subtraction overflow");
    }
    return lhs - rhs;
}

inline static int16_t orange_debug__sub_int16_t(const int16_t lhs, const int16_t rhs, const char *restrict line)
{
    if ((rhs > 0 && lhs < INT16_MIN + rhs) ||
        (rhs < 0 && lhs > INT16_MAX + rhs))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d - %d\n", lhs, rhs);
        orange_debug__panic("subtraction overflow");
    }
    return lhs - rhs;
}

inline static int32_t orange_debug__sub_int32_t(const int32_t lhs, const int32_t rhs, const char *restrict line)
{
    if ((rhs > 0 && lhs < INT32_MIN + rhs) ||
        (rhs < 0 && lhs > INT32_MAX + rhs))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d - %d\n", lhs, rhs);
        orange_debug__panic("subtraction overflow");
    }
    return lhs - rhs;
}

inline static int64_t orange_debug__sub_int64_t(const int64_t lhs, const int64_t rhs, const char *restrict line)
{
    if ((rhs > 0 && lhs < INT64_MIN + rhs) ||
        (rhs < 0 && lhs > INT64_MAX + rhs))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld - %ld\n", lhs, rhs);
        orange_debug__panic("subtraction overflow");
    }
    return lhs - rhs;
}

inline static int8_t orange_debug__mult_int8_t(const int8_t lhs, const int8_t rhs, const char *restrict line)
{
    if (lhs == 0 || rhs == 0)
    {
        return 0;
    }
    else if ((lhs > 0 && ((rhs > 0 && lhs > INT8_MAX / rhs) || (rhs < 0 && rhs < INT8_MIN / lhs))) ||
             (lhs < 0 && ((rhs > 0 && lhs < INT8_MIN / rhs) || (rhs < 0 && rhs < INT8_MAX / lhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d * %d\n", lhs, rhs);
        orange_debug__panic("multiplication overflow");
    }

    return lhs * rhs;
}

inline static int16_t orange_debug__mult_int16_t(const int16_t lhs, const int16_t rhs, const char *restrict line)
{
    if (lhs == 0 || rhs == 0)
    {
        return 0;
    }
    else if ((lhs > 0 && ((rhs > 0 && lhs > INT16_MAX / rhs) || (rhs < 0 && rhs < INT16_MIN / lhs))) ||
             (lhs < 0 && ((rhs > 0 && lhs < INT16_MIN / rhs) || (rhs < 0 && rhs < INT16_MAX / lhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d * %d\n", lhs, rhs);
        orange_debug__panic("multiplication overflow");
    }

    return lhs * rhs;
}

inline static int32_t orange_debug__mult_int32_t(const int32_t lhs, const int32_t rhs, const char *restrict line)
{
    if (lhs == 0 || rhs == 0)
    {
        return 0;
    }
    else if ((lhs > 0 && ((rhs > 0 && lhs > INT32_MAX / rhs) || (rhs < 0 && rhs < INT32_MIN / lhs))) ||
             (lhs < 0 && ((rhs > 0 && lhs < INT32_MIN / rhs) || (rhs < 0 && rhs < INT32_MAX / lhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d * %d\n", lhs, rhs);
        orange_debug__panic("multiplication overflow");
    }

    return lhs * rhs;
}

inline static int64_t orange_debug__mult_int64_t(const int64_t lhs, const int64_t rhs, const char *restrict line)
{
    if (lhs == 0 || rhs == 0)
    {
        return 0;
    }
    else if ((lhs > 0 && ((rhs > 0 && lhs > INT64_MAX / rhs) || (rhs < 0 && rhs < INT64_MIN / lhs))) ||
             (lhs < 0 && ((rhs > 0 && lhs < INT64_MIN / rhs) || (rhs < 0 && rhs < INT64_MAX / lhs))))
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld * %ld\n", lhs, rhs);
        orange_debug__panic("multiplication overflow");
    }

    return lhs * rhs;
}

inline static int8_t orange_debug__div_int8_t(const int8_t lhs, const int8_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT8_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division overflow");
    }

    return lhs / rhs;
}

inline static int16_t orange_debug__div_int16_t(const int16_t lhs, const int16_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT16_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division overflow");
    }

    return lhs / rhs;
}

inline static int32_t orange_debug__div_int32_t(const int32_t lhs, const int32_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT32_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d / %d\n", lhs, rhs);
        orange_debug__panic("division overflow");
    }

    return lhs / rhs;
}

inline static int64_t orange_debug__div_int64_t(const int64_t lhs, const int64_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld / %ld\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT64_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld / %ld\n", lhs, rhs);
        orange_debug__panic("division overflow");
    }

    return lhs / rhs;
}

inline static int8_t orange_debug__mod_int8_t(const int8_t lhs, const int8_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT8_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("modulus overflow");
    }

    return lhs % rhs;
}

inline static int16_t orange_debug__mod_int16_t(const int16_t lhs, const int16_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT16_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("modulus overflow");
    }

    return lhs % rhs;
}

inline static int32_t orange_debug__mod_int32_t(const int32_t lhs, const int32_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT32_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("modulus overflow");
    }

    return lhs % rhs;
}

inline static int64_t orange_debug__mod_int64_t(const int64_t lhs, const int64_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld %% %ld\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }
    else if (lhs == INT64_MIN && rhs == -1)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%ld %% %ld\n", lhs, rhs);
        orange_debug__panic("modulus overflow");
    }

    return lhs % rhs;
}

inline static uint8_t orange_debug__mod_uint8_t(const uint8_t lhs, const uint8_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }

    return lhs % rhs;
}

inline static uint16_t orange_debug__mod_uint16_t(const uint16_t lhs, const uint16_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }

    return lhs % rhs;
}

inline static uint32_t orange_debug__mod_uint32_t(const uint32_t lhs, const uint32_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%d %% %d\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }

    return lhs % rhs;
}

inline static uint64_t orange_debug__mod_uint64_t(const uint64_t lhs, const uint64_t rhs, const char *restrict line)
{
    if (rhs == 0)
    {
        orange_debug__lines[orange_debug__line_idx++] = line;
        fprintf(stderr, "%llu %% %llu\n", lhs, rhs);
        orange_debug__panic("division by zero");
    }

    return lhs % rhs;
}

#endif
