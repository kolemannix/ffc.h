#ifndef JKN_FF_BIGINT_H
#define JKN_FF_BIGINT_H

#include "common.h"

// rust style `try!()` macro, or `?` operator
#define FFC_TRY(x)                                                       \
  {                                                                            \
    if (!(x))                                                                  \
      return false;                                                            \
  }

// the limb width: we want efficient multiplication of double the bits in
// limb, or for 64-bit limbs, at least 64-bit multiplication where we can
// extract the high and low parts efficiently. this is every 64-bit
// architecture except for sparc, which emulates 128-bit multiplication.
// we might have platforms where `CHAR_BIT` is not 8, so let's avoid
// doing `8 * sizeof(limb)`.
#if defined(FFC_64BIT) && !defined(__sparc)
#define FFC_64BIT_LIMB 1
typedef uint64_t limb;
#define JKN_FF_LIMB_BITS 64
#else
#define FFC_32BIT_LIMB
typedef uint32_t limb;
#define JKN_FF_LIMB_BITS 32
#endif

typedef struct { limb* ptr; size_t len; } limb_span;

jkn_ff_internal jkn_ff_inline
limb jkn_ff_limb_span_index(limb_span limb_span, size_t index) {
  JKN_FF_DEBUG_ASSERT(index < limb_span.len);
  return limb_span.ptr[index];
}
#define span_index(span, index) jkn_ff_limb_span_index(span, index)

// number of bits in a bigint. this needs to be at least the number
// of bits required to store the largest bigint, which is
// `log2(10**(digits + max_exp))`, or `log2(10**(767 + 342))`, or
// ~3600 bits, so we round to 4000.
#define JKN_FF_BIGINT_BITS 4000

// vector-like type that is allocated on the stack. the entire
// buffer is pre-allocated, and only the length changes.

// SV_LIMB_COUNT should be 125 or 32-bit systems or 62 for 64-bit systems
#define SV_LIMB_COUNT JKN_FF_BIGINT_BITS / JKN_FF_LIMB_BITS

typedef struct sv {
  limb data[SV_LIMB_COUNT];
  // we never need more than 150 limbs
  uint16_t len;
} sv;

// add items to the vector, from a span, without bounds checking
jkn_ff_internal jkn_ff_inline
void jkn_ff_sv_extend_unchecked(sv* sv, limb_span s) {
  limb *ptr = sv->data + sv->len;

  size_t s_bytes = s.len * sizeof(limb);
  memcpy(ptr, s.ptr, s_bytes);
  sv->len += s.len;
}

// try to add items to the vector, returning if items were added
jkn_ff_internal jkn_ff_inline
bool jkn_ff_sv_try_extend(sv* sv, limb_span s) {
  if (sv->len + s.len <= SV_LIMB_COUNT) {
    jkn_ff_sv_extend_unchecked(sv, s);
    return true;
  } else {
    return false;
  }
}

// create from existing limb span.
jkn_ff_internal jkn_ff_inline
sv jkn_ff_sv_create(limb_span s) {
  sv new_one = {0};
  jkn_ff_sv_try_extend(&new_one, s);
  return new_one;
}

jkn_ff_internal jkn_ff_inline
limb jkn_ff_sv_index(sv sv, size_t index) {
  JKN_FF_DEBUG_ASSERT(index < sv.len);
  return sv.data[index];
}
#define sv_index(sv, index) jkn_ff_sv_index(sv, index)

// index from the end of the container
jkn_ff_internal jkn_ff_inline
limb jkn_ff_sv_rindex(sv sv, size_t index) {
  JKN_FF_DEBUG_ASSERT(index < sv.len);
  size_t rindex = sv.len - index - 1;
  return sv.data[rindex];
}
#define sv_rindex(sv, index) jkn_ff_sv_rindex(sv, index)

// append item to vector, without bounds checking
jkn_ff_internal jkn_ff_inline
void jkn_ff_sv_push_unchecked(sv* sv, limb value) {
  sv->data[sv->len] = value;
  sv->len++;
}

// append item to vector, returning if item was added
jkn_ff_internal jkn_ff_inline
bool jkn_ff_sv_try_push(sv* sv, limb value) {
  if (sv->len < SV_LIMB_COUNT) {
    jkn_ff_sv_push_unchecked(sv, value);
    return true;
  } else {
    return false;
  }
}

// try to reserve new_len limbs, filling with limbs
// FF_DIVERGE: We remove some extra helpers and simply fill with zeros
jkn_ff_internal jkn_ff_inline
bool jkn_ff_sv_try_reserve(sv* sv, size_t new_len) {
  if (new_len > SV_LIMB_COUNT) {
    return false;
  } else {
    if (new_len > sv->len) {
      size_t fill_count = new_len - sv->len;
      limb *first = sv->data + sv->len;
      limb *last = first + fill_count;
      for (limb* p = first; p < last; p++) {
        *p = 0;
      }
      sv->len = (uint16_t)new_len;
    } else {
      sv->len = (uint16_t)new_len;
    }
    return true;
  }
}

// check if any limbs are non-zero after the given index.
jkn_ff_internal jkn_ff_inline
bool jkn_ff_sv_exists_nonzero_after(sv sv, size_t index) {
  while (index < sv.len) {
    if (sv_rindex(sv, index) != 0) {
      return true;
    }
    index++;
  }
  return false;
}

// normalize the big integer, so most-significant zero limbs are removed.
jkn_ff_internal jkn_ff_inline
void jkn_ff_sv_normalize(sv* sv) {
  while (sv->len > 0 && sv_rindex(*sv, 0) == 0) {
    sv->len--;
  }
}

jkn_ff_internal jkn_ff_inline
uint64_t uint64_hi64_1(uint64_t r0, bool* truncated) {
  *truncated = false;
  int shl = (int)jkn_ff_count_leading_zeroes(r0);
  return r0 << shl;
}

jkn_ff_internal jkn_ff_inline
uint64_t uint64_hi64_2(uint64_t r0, uint64_t r1, bool* truncated) {
  int shl = (int)jkn_ff_count_leading_zeroes(r0);
  if (shl == 0) {
    *truncated = r1 != 0;
    return r0;
  } else {
    int shr = 64 - shl;
    *truncated = (r1 << shl) != 0;
    return (r0 << shl) | (r1 >> shr);
  }
}

jkn_ff_internal jkn_ff_inline
uint64_t uint32_hi64_1(uint32_t r0, bool* truncated) {
  return uint64_hi64_1(r0, truncated);
}

jkn_ff_internal jkn_ff_inline
uint64_t uint32_hi64_2(uint32_t r0, uint32_t r1, bool* truncated) {
  uint64_t x0 = r0;
  uint64_t x1 = r1;
  return uint64_hi64_1((x0 << 32) | x1, truncated);
}

jkn_ff_internal jkn_ff_inline
uint64_t uint32_hi64_3(uint32_t r0, uint32_t r1, uint32_t r2, bool* truncated) {
  uint64_t x0 = r0;
  uint64_t x1 = r1;
  uint64_t x2 = r2;
  return uint64_hi64_2(x0, (x1 << 32) | x2, truncated);
}

// add two small integers, checking for overflow.
// we want an efficient operation. for msvc, where
// we don't have built-in intrinsics, this is still
// pretty fast.
jkn_ff_internal jkn_ff_inline
limb jkn_ff_bigint_scalar_add(limb x, limb y, bool* overflow) {
  limb z;
// gcc and clang
#if defined(__has_builtin)
#if __has_builtin(__builtin_add_overflow)
  *overflow = __builtin_add_overflow(x, y, &z);
  return z;
#endif
#endif

  // generic, this still optimizes correctly on MSVC.
  z = x + y;
  *overflow = z < x;
  return z;
}

// multiply two small integers, getting both the high and low bits.
jkn_ff_inline limb
scalar_mul(limb x, limb y, limb* carry) {
#ifdef FFC_64BIT_LIMB
#if defined(__SIZEOF_INT128__)
  // GCC and clang both define it as an extension.
  __uint128_t z = (__uint128_t)(x) * (__uint128_t)(y) + (__uint128_t)(*carry);
  *carry = (limb)(z >> JKN_FF_LIMB_BITS);
  return (limb)(z);
#else
  // fallback, no native 128-bit integer multiplication with carry.
  // on msvc, this optimizes identically, somehow.
  jkn_ff_u128 z = jkn_ff_full_multiplication(x, y);
  bool overflow;
  z.low = scalar_add(z.low, *carry, &overflow);
  z.high += (uint64_t)(overflow); // cannot overflow
  *carry = z.high;
  return z.low;
#endif
#else
  uint64_t z = (uint64_t)(x) * (uint64_t)(y) + (uint64_t)(*carry);
  *carry = (limb)(z >> limb_bits);
  return (limb)(z);
#endif
}

// add scalar value to bigint starting from offset.
// used in grade school multiplication
jkn_ff_internal jkn_ff_inline
bool small_add_from(sv* sv, limb y, size_t start) {
  size_t index = start;
  limb carry = y;
  bool overflow;
  while (carry != 0 && index < sv->len) {
    sv->data[index] = jkn_ff_bigint_scalar_add(sv->data[index], carry, &overflow);
    carry = (limb)(overflow);
    index += 1;
  }
  if (carry != 0) {
    FFC_TRY(jkn_ff_sv_try_push(sv, carry));
  }
  return true;
}

// add scalar value to bigint.
jkn_ff_internal jkn_ff_inline
bool jkn_ff_bigint_small_add(sv* sv, limb y) {
  return small_add_from(sv, y, 0);
}

// multiply bigint by scalar value.
jkn_ff_internal jkn_ff_inline
bool jkn_ff_bigint_small_mul(sv* sv, limb y) {
  limb carry = 0;
  for (size_t index = 0; index < sv->len; index++) {
    sv->data[index] = scalar_mul(sv->data[index], y, &carry);
  }
  if (carry != 0) {
    FFC_TRY(jkn_ff_sv_try_push(sv, carry));
  }
  return true;
}

// add bigint to bigint starting from index.
// used in grade school multiplication
bool large_add_from(sv* x, limb_span y, size_t start) {
  // the effective x buffer is from `xstart..x.len()`, so exit early
  // if we can't get that current range.

  // JKN_FF_DIVERGE: We are calling our try_reserve instead of the o.g. try_resize
  if (x->len < start || y.len > x->len - start) {
    FFC_TRY(jkn_ff_sv_try_reserve(x, y.len + start));
  }

  bool carry = false;
  for (size_t index = 0; index < y.len; index++) {
    limb xi = x->data[index + start];
    limb yi = span_index(y, index);
    bool c1 = false;
    bool c2 = false;
    xi = jkn_ff_bigint_scalar_add(xi, yi, &c1);
    if (carry) {
      xi = jkn_ff_bigint_scalar_add(xi, 1, &c2);
    }
    x->data[index + start] = xi;
    carry = c1 | c2;
  }

  // handle overflow
  if (carry) {
    FFC_TRY(small_add_from(x, 1, y.len + start));
  }
  return true;
}

// add bigint to bigint.
jkn_ff_inline bool jkn_ff_sv_large_add_from(sv* x, limb_span y) {
  return large_add_from(x, y, 0);
}

// grade-school multiplication algorithm
bool long_mul(sv* x, limb_span y) {
  limb_span xs = (limb_span){ .ptr = x->data, .len = x->len };

  // full copy of x into z
  sv z = jkn_ff_sv_create(xs);

  limb_span zs = (limb_span){ .ptr = z.data, .len = z.len };

  if (y.len != 0) {
    limb y0 = span_index(y, 0);
    FFC_TRY(jkn_ff_bigint_small_mul(x, y0));
    for (size_t index = 1; index < y.len; index++) {

      limb yi = span_index(y, index);
      sv zi; // re-use the same buffer throughout

      if (yi != 0) {
        zi.len = 0;
        FFC_TRY(jkn_ff_sv_try_extend(&zi, zs));
        FFC_TRY(jkn_ff_bigint_small_mul(&zi, yi));
        limb_span zis = (limb_span){zi.data, zi.len};
        FFC_TRY(large_add_from(x, zis, index));
      }
    }
  }

  jkn_ff_sv_normalize(x);
  return true;
}

// grade-school multiplication algorithm
bool large_mul(sv* x, limb_span y) {
  if (y.len == 1) {
    FFC_TRY(jkn_ff_bigint_small_mul(x, span_index(y,0)));
  } else {
    FFC_TRY(long_mul(x, y));
  }
  return true;
}

static const uint32_t pow5_tables_large_step = 135;
static const uint64_t pow5_tables_small_powers[] = {
    1UL,
    5UL,
    25UL,
    125UL,
    625UL,
    3125UL,
    15625UL,
    78125UL,
    390625UL,
    1953125UL,
    9765625UL,
    48828125UL,
    244140625UL,
    1220703125UL,
    6103515625UL,
    30517578125UL,
    152587890625UL,
    762939453125UL,
    3814697265625UL,
    19073486328125UL,
    95367431640625UL,
    476837158203125UL,
    2384185791015625UL,
    11920928955078125UL,
    59604644775390625UL,
    298023223876953125UL,
    1490116119384765625UL,
    7450580596923828125UL,
};
#ifdef FFC_64BIT_LIMB
  static const limb jkn_ff_large_power_of_5[] = {
      1414648277510068013UL, 9180637584431281687UL, 4539964771860779200UL,
      10482974169319127550UL, 198276706040285095UL};
#else
  static const limb jkn_ff_large_power_of_5[] = {
      4279965485U, 329373468U,  4020270615U, 2137533757U, 4287402176U,
      1057042919U, 1071430142U, 2440757623U, 381945767U,  46164893U};
#endif

// big integer type. implements a small subset of big integer
// arithmetic, using simple algorithms since asymptotically
// faster algorithms are slower for a small number of limbs.
// all operations assume the big-integer is normalized.
typedef struct jkn_ff_bigint {
  // storage of the limbs, in little-endian order.
  sv vec;
} jkn_ff_bigint;

jkn_ff_bigint jkn_ff_bigint_empty() {
  sv sv;
  sv.len = 0;
  return (jkn_ff_bigint){sv};
}

jkn_ff_bigint jkn_ff_bigint_make(uint64_t value) {
  sv sv;
  sv.len = 0;
#ifdef FFC_64BIT_LIMB
  jkn_ff_sv_push_unchecked(&sv, value);
#else
  jkn_ff_sv_push_unchecked(&sv, uint32_t(value));
  jkn_ff_sv_push_unchecked(&sv, uint32_t(value >> 32));
#endif
  jkn_ff_sv_normalize(&sv);
  return (jkn_ff_bigint){sv};
}

// get the high 64 bits from the vector, and if bits were truncated.
// this is to get the significant digits for the float.
uint64_t jkn_ff_bigint_hi64(jkn_ff_bigint me, bool* truncated) {
  sv vec = me.vec;
#ifdef FFC_64BIT_LIMB
  if (vec.len == 0) {
    *truncated = false;
    return 0;
  } else if (vec.len == 1) {
    return uint64_hi64_1(sv_rindex(vec,0), truncated);
  } else {
    uint64_t result = uint64_hi64_2(sv_rindex(vec, 0), sv_rindex(vec, 1), truncated);
    *truncated |= jkn_ff_sv_exists_nonzero_after(vec, 2);
    return result;
  }
#else
  if (vec.len == 0) {
    *truncated = false;
    return 0;
  } else if (vec.len == 1) {
    return uint32_hi64_1(sv_rindex(vec,0), truncated);
  } else if (vec.len == 2) {
    return uint32_hi64_2(sv_rindex(vec,0), sv_rindex(vec,1), truncated);
  } else {
    uint64_t result = uint32_hi64_3(
        sv_rindex(vec,0), 
        sv_rindex(vec,1), 
        sv_rindex(vec,2),
        truncated
    );
    *truncated |= jkn_ff_sv_exists_nonzero_after(vec , 3);
    return result;
  }
#endif
}

// compare two big integers, returning the large value.
// assumes both are normalized. if the return value is
// negative, other is larger, if the return value is
// positive, this is larger, otherwise they are equal.
// the limbs are stored in little-endian order, so we
// must compare the limbs in ever order.
jkn_ff_internal jkn_ff_inline 
int jkn_ff_bigint_compare(jkn_ff_bigint me, jkn_ff_bigint const *other) {
  if (me.vec.len > other->vec.len) {
    return 1;
  } else if (me.vec.len < other->vec.len) {
    return -1;
  } else {
    for (size_t index = me.vec.len; index > 0; index--) {
      limb xi = sv_index(me.vec, index - 1);
      limb yi = sv_index(other->vec, index - 1);
      if (xi > yi) {
        return 1;
      } else if (xi < yi) {
        return -1;
      }
    }
    return 0;
  }
}

// shift left each limb n bits, carrying over to the new limb
// returns true if we were able to shift all the digits.
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_shl_bits(jkn_ff_bigint* me, size_t n) {
  // Internally, for each item, we shift left by n, and add the previous
  // right shifted limb-bits.
  // For example, we transform (for u8) shifted left 2, to:
  //      b10100100 b01000010
  //      b10 b10010001 b00001000
  JKN_FF_DEBUG_ASSERT(n != 0);
  JKN_FF_DEBUG_ASSERT(n < sizeof(limb) * 8);

  size_t shl = n;
  size_t shr = JKN_FF_LIMB_BITS - shl;
  limb prev = 0;
  for (size_t index = 0; index < me->vec.len; index++) {
    limb xi = sv_index(me->vec, index);
    me->vec.data[index] = (xi << shl) | (prev >> shr);
    prev = xi;
  }

  limb carry = prev >> shr;
  if (carry != 0) {
    return jkn_ff_sv_try_push(&me->vec, carry);
  }
  return true;
}

// move the limbs left by `n` limbs.
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_shl_limbs(jkn_ff_bigint* me, size_t n) {
  JKN_FF_DEBUG_ASSERT(n != 0);
  if (n + me->vec.len > SV_LIMB_COUNT) {
    return false;
  } else if (me->vec.len != 0) {
    // move limbs
    limb *dst = me->vec.data + n;
    limb const *src = me->vec.data;
    // std::copy_backward(src, src + vec.len(), dst + vec.len());
    // memmove to handle the overlap
    memmove(dst, src, me->vec.len * sizeof(limb));
    
    // fill in empty limbs
    limb *first = me->vec.data;
    // limb *last = first + n;
    // ::std::fill(first, last, 0);
    memset(first, 0, n * sizeof(limb));
    me->vec.len += n;
    return true;
  } else {
    return true;
  }
}

// move the limbs left by `n` bits.
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_shl(jkn_ff_bigint* me, size_t n) {
  size_t rem = n % JKN_FF_LIMB_BITS;
  size_t div = n / JKN_FF_LIMB_BITS;
  if (rem != 0) {
    FFC_TRY(jkn_ff_bigint_shl_bits(me, rem));
  }
  if (div != 0) {
    FFC_TRY(jkn_ff_bigint_shl_limbs(me, div));
  }
  return true;
}

// get the number of leading zeros in the bigint.
jkn_ff_internal jkn_ff_inline 
int jkn_ff_bigint_ctlz(jkn_ff_bigint me) {
  if (me.vec.len == 0) {
    return 0;
  } else {
#ifdef FFC_64BIT_LIMB
    return (int)jkn_ff_count_leading_zeroes(sv_rindex(me.vec, 0));
#else
    // no use defining a specialized count_leading_zeros for a 32-bit type.
    uint64_t r0 = sv_rindex(me.vec, 0);
    return jkn_ff_count_leading_zeroes(r0 << 32);
#endif
  }
}

// get the number of bits in the bigint.
jkn_ff_internal jkn_ff_inline 
int jkn_ff_bigint_bit_length(jkn_ff_bigint me) {
  int lz = jkn_ff_bigint_ctlz(me);
  return (int)(JKN_FF_LIMB_BITS * me.vec.len) - lz;
}

jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_mul(jkn_ff_bigint* me, limb y) { return jkn_ff_bigint_small_mul(&me->vec, y); }

jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_add(jkn_ff_bigint* me, limb y) { return jkn_ff_bigint_small_add(&me->vec, y); }

// multiply as if by 2 raised to a power.
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_pow2(jkn_ff_bigint* me, uint32_t exp) { return jkn_ff_bigint_shl(me, exp); }

// multiply as if by 5 raised to a power.
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_pow5(jkn_ff_bigint* me, uint32_t exp) {
  // multiply by a power of 5
  size_t large_length = sizeof(jkn_ff_large_power_of_5) / sizeof(limb);
  limb_span large = (limb_span){ .ptr = (limb*)jkn_ff_large_power_of_5, .len = large_length};
  while (exp >= pow5_tables_large_step) {
    FFC_TRY(large_mul(&me->vec, large));
    exp -= pow5_tables_large_step;
  }
#ifdef FFC_64BIT_LIMB
  uint32_t small_step = 27;
  limb max_native = 7450580596923828125UL;
#else
  uint32_t small_step = 13;
  limb max_native = 1220703125U;
#endif
  while (exp >= small_step) {
    FFC_TRY(jkn_ff_bigint_small_mul(&me->vec, max_native));
    exp -= small_step;
  }
  if (exp != 0) {
    FFC_TRY(
      jkn_ff_bigint_small_mul(&me->vec, (limb)(pow5_tables_small_powers[exp]))
    );
  }

  return true;
}

// multiply as if by 10 raised to a power.
// nocommit audit for exported symbols
jkn_ff_internal jkn_ff_inline 
bool jkn_ff_bigint_pow10(jkn_ff_bigint* me, uint32_t exp) {
  FFC_TRY(jkn_ff_bigint_pow5(me, exp));
  return jkn_ff_bigint_pow2(me, exp);
}

#undef span_index
#undef sv_rindex

#endif // JKN_FF_BIGINT_H
