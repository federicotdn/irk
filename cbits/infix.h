#ifndef INFIX_H
#define INFIX_H

#include <stdbool.h>
#include <stdint.h>

bool has_avx2();

bool is_infix_of(const uint8_t *needle, int64_t needle_len,
                 const uint8_t *haystack, int64_t haystack_len);

#endif
