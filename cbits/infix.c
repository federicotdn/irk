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

bool is_infix_of(const uint8_t *needle, int64_t needle_len,
                 const uint8_t *haystack, int64_t haystack_len) {
  if (needle_len == 0) return true;
  if (needle_len > haystack_len) return false;

  int64_t limit = haystack_len - needle_len;

  // Skip AVX if haystack is too short
  if (limit < 32) {
    for (int64_t i = 0; i <= limit; i++) {
      if (memcmp(haystack + i, needle, needle_len) == 0) {
        return true;
      }
    }
    return false;
  }

  // Broadcast first and last bytes of needle
  __m256i first = _mm256_set1_epi8(needle[0]);
  __m256i last = _mm256_set1_epi8(needle[needle_len - 1]);

  int64_t i = 0;
  int64_t simd_limit = limit - 31;  // need 32 bytes for AVX2 load

  for (; i <= simd_limit; i += 32) {
    // Load 32 bytes at position i and at position i + needle_len - 1
    __m256i block_first = _mm256_loadu_si256((const __m256i *)(haystack + i));
    __m256i block_last = _mm256_loadu_si256((const __m256i *)(haystack + i + needle_len - 1));

    // Compare against first/last byte of needle
    __m256i eq_first = _mm256_cmpeq_epi8(first, block_first);
    __m256i eq_last = _mm256_cmpeq_epi8(last, block_last);

    // Positions where both first AND last byte match
    uint32_t mask = _mm256_movemask_epi8(_mm256_and_si256(eq_first, eq_last));

    // Check each candidate position
    while (mask) {
      int bitpos = __builtin_ctz(mask);
      if (memcmp(haystack + i + bitpos, needle, needle_len) == 0) {
        return true;
      }
      mask &= mask - 1;  // Clear lowest set bit
    }
  }

  // Do the rest (if any) using memcmp
  for (; i <= limit; i++) {
    if (memcmp(haystack + i, needle, needle_len) == 0) {
      return true;
    }
  }

  return false;
}

#else
// Fallback for non-AVX2 platforms

bool has_avx2() { return false; }

bool is_infix_of(const uint8_t *needle, int64_t needle_len,
                 const uint8_t *haystack, int64_t haystack_len) {
  return false;
}

#endif
