#include <stdint.h>

const char *orange_debug__lines[1024];
// pointer to current line in stack trace
uint16_t orange_debug__line_idx = 0;
// pointer to top of stacktrace, where an error might be returned from
uint16_t orange_debug__err_line_idx = 0;
