#ifndef INFIX_H
#define INFIX_H

#include <stdbool.h>
#include <stdint.h>

bool has_avx2();

int64_t is_infix_of(const char *needle, const int64_t needle_size,
                    const char *haystack, const int64_t haystack_size);

#endif
