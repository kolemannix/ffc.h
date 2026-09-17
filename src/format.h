#ifndef FFC_FORMAT_H
#define FFC_FORMAT_H

#include "common.h"
#include "bigint.h"

/* section: binary to decimal (fixed notation)
 *
 * Exact fixed-notation formatting, equivalent to printf("%.*f") on a libc
 * that prints the exact binary value (glibc, musl, macOS). No floating-point
 * arithmetic is used, so the result does not depend on the FPU rounding mode
 * and is bit-for-bit reproducible across platforms.
 *
 * A double is m * 2^e with m < 2^53. To print `places` fraction digits we
 * need N = round(m * 2^e * 10^places), then print N with the decimal point
 * `places` digits from the right.
 *
 *   e >= 0: the value is the integer m << e, every fraction digit is zero.
 *   e <  0: let k = -e. Only the first k fraction digits can be nonzero, so
 *           with p = min(places, k):  N = round((m * 5^p) >> (k - p))
 *           and the remaining places - p digits are zero.
 *
 * The shift-right is where rounding happens: the dropped bits give the exact
 * half/sticky information for round-half-to-even. The worst case is
 * m * 5^1074, about 2550 bits, which the 4000-bit ffc_bigint holds easily.
 * The work is bounded by `places`, not by the exponent, so small numbers
 * printed with few places stay cheap.
 */

// Divide in place by a divisor below 2^32, returning the remainder.
// Each 64-bit limb is split into 32-bit halves so the only division needed
// is 64-by-32, which is native everywhere and never a compiler-rt libcall.
ffc_internal
uint32_t ffc_bigint_divmod_u32(ffc_bigint* me, uint32_t d) {
  uint64_t rem = 0;
  for (size_t index = me->vec.len; index > 0; index--) {
    ffc_bigint_limb x = me->vec.data[index - 1];
#ifdef FFC_64BIT_LIMB
    uint64_t hi = (rem << 32) | (x >> 32);
    uint64_t qhi = hi / d;
    rem = hi % d;
    uint64_t lo = (rem << 32) | (x & 0xFFFFFFFFu);
    uint64_t qlo = lo / d;
    rem = lo % d;
    me->vec.data[index - 1] = (qhi << 32) | qlo;
#else
    uint64_t cur = (rem << 32) | x;
    me->vec.data[index - 1] = (ffc_bigint_limb)(cur / d);
    rem = cur % d;
#endif
  }
  ffc_sv_normalize(&me->vec);
  return (uint32_t)rem;
}

// Test bit `n` (0 = least significant).
ffc_internal ffc_inline
bool ffc_bigint_test_bit(ffc_bigint const* me, size_t n) {
  size_t limb = n / FFC_LIMB_BITS;
  if (limb >= me->vec.len) {
    return false;
  }
  return ((me->vec.data[limb] >> (n % FFC_LIMB_BITS)) & 1) != 0;
}

// True if any of the bits [0, n) is set.
ffc_internal
bool ffc_bigint_any_bits_below(ffc_bigint const* me, size_t n) {
  size_t whole = n / FFC_LIMB_BITS;
  size_t rem = n % FFC_LIMB_BITS;
  if (whole > me->vec.len) {
    whole = me->vec.len;
    rem = 0;
  }
  for (size_t index = 0; index < whole; index++) {
    if (me->vec.data[index] != 0) {
      return true;
    }
  }
  if (rem != 0 && whole < me->vec.len) {
    ffc_bigint_limb mask = (((ffc_bigint_limb)1) << rem) - 1;
    return (me->vec.data[whole] & mask) != 0;
  }
  return false;
}

// Shift right by `n` bits, discarding the low bits.
ffc_internal
void ffc_bigint_shr(ffc_bigint* me, size_t n) {
  size_t limbs = n / FFC_LIMB_BITS;
  size_t bits = n % FFC_LIMB_BITS;
  if (limbs >= me->vec.len) {
    me->vec.len = 0;
    return;
  }
  if (limbs != 0) {
    size_t keep = me->vec.len - limbs;
    memmove(me->vec.data, me->vec.data + limbs, keep * sizeof(ffc_bigint_limb));
    me->vec.len = (uint16_t)keep;
  }
  if (bits != 0) {
    size_t last = me->vec.len - 1;
    for (size_t index = 0; index < last; index++) {
      me->vec.data[index] = (me->vec.data[index] >> bits) |
                            (me->vec.data[index + 1] << (FFC_LIMB_BITS - bits));
    }
    me->vec.data[last] >>= bits;
  }
  ffc_sv_normalize(&me->vec);
}

// Bounded output cursor. `len` always counts the full output; bytes past
// `cap` are dropped so the caller can size a buffer from the return value.
typedef struct ffc_writer {
  char *buf;
  size_t cap;
  size_t len;
} ffc_writer;

ffc_internal ffc_inline
void ffc_writer_put(ffc_writer* w, char c) {
  if (w->len < w->cap) {
    w->buf[w->len] = c;
  }
  w->len++;
}

ffc_internal ffc_inline
void ffc_writer_put_str(ffc_writer* w, char const* s) {
  while (*s) {
    ffc_writer_put(w, *s++);
  }
}

ffc_internal ffc_inline
void ffc_writer_put_zeros(ffc_writer* w, size_t n) {
  while (n--) {
    ffc_writer_put(w, '0');
  }
}

// Largest N we can produce is just under 2^53 * 5^1074, which has 767
// decimal digits (768 after a round-up). Digits are pulled nine at a time,
// so round up to a multiple of nine with slack.
#define FFC_FORMAT_DIGIT_BUF 800

size_t ffc_format_double_fixed(char *buf, size_t cap, double value, int places) {
  ffc_writer w;
  w.buf = buf;
  w.cap = cap;
  w.len = 0;

  size_t p = places < 0 ? 6 : (size_t)places;

  uint64_t bits = ffc_get_double_bits(value);
  bool negative = (bits >> FFC_DOUBLE_SIGN_INDEX) != 0;
  int32_t biased_exp = (int32_t)((bits & FFC_DOUBLE_EXPONENT_MASK) >> FFC_DOUBLE_MANTISSA_EXPLICIT_BITS);
  uint64_t m = bits & FFC_DOUBLE_MANTISSA_MASK;

  if (negative) {
    ffc_writer_put(&w, '-');
  }
  if (biased_exp == FFC_DOUBLE_INFINITE_POWER) {
    ffc_writer_put_str(&w, m == 0 ? "inf" : "nan");
    return w.len;
  }

  int32_t e;
  if (biased_exp == 0) {
    e = FFC_DOUBLE_MINIMUM_EXPONENT + 1 - FFC_DOUBLE_MANTISSA_EXPLICIT_BITS; // subnormal: -1074
  } else {
    m |= FFC_DOUBLE_HIDDEN_BIT_MASK;
    e = biased_exp + FFC_DOUBLE_MINIMUM_EXPONENT - FFC_DOUBLE_MANTISSA_EXPLICIT_BITS;
  }
  if (m == 0) {
    e = 0;
  }
  // Strip trailing zero bits so k is as small as it can be.
  while (e < 0 && (m & 1) == 0) {
    m >>= 1;
    e++;
  }

  // N = big, holding p_eff fraction digits.
  ffc_bigint big = ffc_bigint_make(m);
  size_t p_eff = 0;
  if (e >= 0) {
    if (m != 0) {
      FFC_ASSERT(ffc_bigint_shl(&big, (size_t)e));
    }
  } else {
    size_t k = (size_t)(-e);
    p_eff = p < k ? p : k;
    FFC_ASSERT(ffc_bigint_pow5(&big, (uint32_t)p_eff));
    size_t drop = k - p_eff;
    if (drop != 0) {
      bool half = ffc_bigint_test_bit(&big, drop - 1);
      bool sticky = ffc_bigint_any_bits_below(&big, drop - 1);
      ffc_bigint_shr(&big, drop);
      bool odd = big.vec.len != 0 && (big.vec.data[0] & 1) != 0;
      if (half && (sticky || odd)) {
        FFC_ASSERT(ffc_bigint_add(&big, 1));
      }
    }
  }

  // Extract the decimal digits of N, least significant first.
  char digits[FFC_FORMAT_DIGIT_BUF];
  size_t ndigits = 0;
  while (big.vec.len != 0) {
    uint32_t chunk = ffc_bigint_divmod_u32(&big, 1000000000u);
    for (int i = 0; i < 9; i++) {
      digits[ndigits++] = (char)('0' + chunk % 10);
      chunk /= 10;
    }
  }
  while (ndigits > 0 && digits[ndigits - 1] == '0') {
    ndigits--;
  }

  // Integer part: everything above the p_eff fraction digits.
  if (ndigits > p_eff) {
    for (size_t i = ndigits; i > p_eff; i--) {
      ffc_writer_put(&w, digits[i - 1]);
    }
  } else {
    ffc_writer_put(&w, '0');
  }

  if (p != 0) {
    ffc_writer_put(&w, '.');
    size_t have = ndigits < p_eff ? ndigits : p_eff;
    ffc_writer_put_zeros(&w, p_eff - have);
    for (size_t i = have; i > 0; i--) {
      ffc_writer_put(&w, digits[i - 1]);
    }
    ffc_writer_put_zeros(&w, p - p_eff);
  }
  return w.len;
}

#undef FFC_FORMAT_DIGIT_BUF

/* end section: binary to decimal */

#endif // FFC_FORMAT_H
