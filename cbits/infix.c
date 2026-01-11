/*
 * AVX2 naming guide
 *
 * _mm256      As prefix: 256-bit register (_mm=128/SSE, _mm512=512/AVX-512)
 * __m256i     As type: integer vector (no suffix=float, d=double, i=integer)
 *
 * ops
 *   set1      Broadcast single value to all lanes
 *   loadu     Load unaligned
 *   cmpeq     Compare equal -> places 0xFF on matches
 *   movemask  Extract high bit of each lane into scalar bitmask
 *
 * suffixes
 *   epi8/16/32/64   Signed packed integers (8=32 lanes, 16=16, 32=8, 64=4)
 *   si256           Whole register as opaque blob (Scalar Integer 256 bits)
 */

#include "infix.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#if defined(__x86_64__) && defined(__AVX2__)
#include <immintrin.h>

bool has_avx2() { return true; }

// Based on:
// http://0x80.pl/notesen/2016-11-28-simd-strfind.html#algorithm-1-generic-simd
int64_t is_infix_of(const char *needle, const int64_t needle_size,
                    const char *haystack, const int64_t haystack_size) {
  if (needle_size == 0) {
    return 0;
  } else if (needle_size > haystack_size) {
    return -1;
  }

  const __m256i first = _mm256_set1_epi8(needle[0]);
  const __m256i last = _mm256_set1_epi8(needle[needle_size - 1]);

  const size_t simd_limit =
      haystack_size >= needle_size + 31 ? haystack_size - needle_size - 31 : 0;
  size_t i = 0;

  for (; i < simd_limit; i += 32) {
    const __m256i block_first =
        _mm256_loadu_si256((const __m256i *)(haystack + i));
    const __m256i block_last =
        _mm256_loadu_si256((const __m256i *)(haystack + i + needle_size - 1));

    const __m256i eq_first = _mm256_cmpeq_epi8(first, block_first);
    const __m256i eq_last = _mm256_cmpeq_epi8(last, block_last);

    uint32_t mask = _mm256_movemask_epi8(_mm256_and_si256(eq_first, eq_last));

    while (mask != 0) {
      const int bitpos = __builtin_ctz(mask);

      if (memcmp(haystack + i + bitpos + 1, needle + 1, needle_size - 2) == 0) {
        return i + bitpos;
      }

      // Clear lowest set bit
      mask &= mask - 1;
    }
  }

  for (; i <= haystack_size - needle_size; i++) {
    if (memcmp(haystack + i, needle, needle_size) == 0) {
      return i;
    }
  }

  return -1;
}

#else
// Fallback for non-AVX2 platforms

bool has_avx2() { return false; }

bool is_infix_of(const uint8_t *needle, int64_t needle_len,
                 const uint8_t *haystack, int64_t haystack_len) {
  return false;
}

#endif
