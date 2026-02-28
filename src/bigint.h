#ifndef FFC_BIGINT_H
#define FFC_BIGINT_H

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
typedef uint64_t ffc_bigint_limb;
#define FFC_LIMB_BITS 64
#else
#define FFC_32BIT_LIMB
typedef uint32_t ffc_bigint_limb;
#define FFC_LIMB_BITS 32
#endif

typedef struct { ffc_bigint_limb* ptr; size_t len; } ffc_bigint_limb_span;

ffc_internal ffc_inline
ffc_bigint_limb ffc_limb_span_index(ffc_bigint_limb_span limb_span, size_t index) {
  FFC_DEBUG_ASSERT(index < limb_span.len);
  return limb_span.ptr[index];
}
#define ffc_span_index(span, index) ffc_limb_span_index(span, index)

// number of bits in a bigint. this needs to be at least the number
// of bits required to store the largest bigint, which is
// `log2(10**(digits + max_exp))`, or `log2(10**(767 + 342))`, or
// ~3600 bits, so we round to 4000.
#define FFC_BIGINT_BITS 4000

// vector-like type that is allocated on the stack. the entire
// buffer is pre-allocated, and only the length changes.

// SV_LIMB_COUNT should be 125 or 32-bit systems or 62 for 64-bit systems
#define SV_LIMB_COUNT FFC_BIGINT_BITS / FFC_LIMB_BITS

typedef struct ffc_sv {
  ffc_bigint_limb data[SV_LIMB_COUNT];
  // we never need more than 150 limbs
  uint16_t len;
} ffc_sv;

// add items to the vector, from a span, without bounds checking
ffc_internal ffc_inline
void ffc_sv_extend_unchecked(ffc_sv* sv, ffc_bigint_limb_span s) {
  ffc_bigint_limb *ptr = sv->data + sv->len;

  size_t s_bytes = s.len * sizeof(ffc_bigint_limb);
  memcpy(ptr, s.ptr, s_bytes);
  sv->len += s.len;
}

// try to add items to the vector, returning if items were added
ffc_internal ffc_inline
bool ffc_sv_try_extend(ffc_sv* sv, ffc_bigint_limb_span s) {
  if (sv->len + s.len <= SV_LIMB_COUNT) {
    ffc_sv_extend_unchecked(sv, s);
    return true;
  } else {
    return false;
  }
}

// create from existing limb span.
ffc_internal ffc_inline
ffc_sv ffc_sv_create(ffc_bigint_limb_span s) {
  ffc_sv new_one = {0};
  ffc_sv_try_extend(&new_one, s);
  return new_one;
}

ffc_internal ffc_inline
ffc_bigint_limb ffc_sv_index(ffc_sv sv, size_t index) {
  FFC_DEBUG_ASSERT(index < sv.len);
  return sv.data[index];
}

// index from the end of the container
ffc_internal ffc_inline
ffc_bigint_limb ffc_sv_rindex(ffc_sv sv, size_t index) {
  FFC_DEBUG_ASSERT(index < sv.len);
  size_t rindex = sv.len - index - 1;
  return sv.data[rindex];
}

// append item to vector, without bounds checking
ffc_internal ffc_inline
void ffc_sv_push_unchecked(ffc_sv* sv, ffc_bigint_limb value) {
  sv->data[sv->len] = value;
  sv->len++;
}

// append item to vector, returning if item was added
ffc_internal ffc_inline
bool ffc_sv_try_push(ffc_sv* sv, ffc_bigint_limb value) {
  if (sv->len < SV_LIMB_COUNT) {
    ffc_sv_push_unchecked(sv, value);
    return true;
  } else {
    return false;
  }
}

// try to reserve new_len limbs, filling with limbs
// FF_DIVERGE: We remove some extra helpers and simply fill with zeros
ffc_internal ffc_inline
bool ffc_sv_try_reserve(ffc_sv* sv, size_t new_len) {
  if (new_len > SV_LIMB_COUNT) {
    return false;
  } else {
    if (new_len > sv->len) {
      size_t fill_count = new_len - sv->len;
      ffc_bigint_limb *first = sv->data + sv->len;
      ffc_bigint_limb *last = first + fill_count;
      for (ffc_bigint_limb* p = first; p < last; p++) {
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
ffc_internal ffc_inline
bool ffc_sv_exists_nonzero_after(ffc_sv sv, size_t index) {
  while (index < sv.len) {
    if (ffc_sv_rindex(sv, index) != 0) {
      return true;
    }
    index++;
  }
  return false;
}

// normalize the big integer, so most-significant zero limbs are removed.
ffc_internal ffc_inline
void ffc_sv_normalize(ffc_sv* sv) {
  while (sv->len > 0 && ffc_sv_rindex(*sv, 0) == 0) {
    sv->len--;
  }
}

ffc_internal ffc_inline
uint64_t ffc_uint64_hi64_1(uint64_t r0, bool* truncated) {
  *truncated = false;
  int shl = (int)ffc_count_leading_zeroes(r0);
  return r0 << shl;
}

ffc_internal ffc_inline
uint64_t ffc_uint64_hi64_2(uint64_t r0, uint64_t r1, bool* truncated) {
  int shl = (int)ffc_count_leading_zeroes(r0);
  if (shl == 0) {
    *truncated = r1 != 0;
    return r0;
  } else {
    int shr = 64 - shl;
    *truncated = (r1 << shl) != 0;
    return (r0 << shl) | (r1 >> shr);
  }
}

ffc_internal ffc_inline
uint64_t ffc_uint32_hi64_1(uint32_t r0, bool* truncated) {
  return ffc_uint64_hi64_1(r0, truncated);
}

ffc_internal ffc_inline
uint64_t ffc_uint32_hi64_2(uint32_t r0, uint32_t r1, bool* truncated) {
  uint64_t x0 = r0;
  uint64_t x1 = r1;
  return ffc_uint64_hi64_1((x0 << 32) | x1, truncated);
}

ffc_internal ffc_inline
uint64_t ffc_uint32_hi64_3(uint32_t r0, uint32_t r1, uint32_t r2, bool* truncated) {
  uint64_t x0 = r0;
  uint64_t x1 = r1;
  uint64_t x2 = r2;
  return ffc_uint64_hi64_2(x0, (x1 << 32) | x2, truncated);
}

// add two small integers, checking for overflow.
// we want an efficient operation. for msvc, where
// we don't have built-in intrinsics, this is still
// pretty fast.
ffc_internal ffc_inline
ffc_bigint_limb ffc_bigint_scalar_add(ffc_bigint_limb x, ffc_bigint_limb y, bool* overflow) {
  ffc_bigint_limb z;
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
ffc_inline ffc_internal
ffc_bigint_limb ffc_bigint_scalar_mul(ffc_bigint_limb x, ffc_bigint_limb y, ffc_bigint_limb* carry) {
#ifdef FFC_64BIT_LIMB
#if defined(__SIZEOF_INT128__)
  // GCC and clang both define it as an extension.
  __uint128_t z = (__uint128_t)(x) * (__uint128_t)(y) + (__uint128_t)(*carry);
  *carry = (ffc_bigint_limb)(z >> FFC_LIMB_BITS);
  return (ffc_bigint_limb)(z);
#else
  // fallback, no native 128-bit integer multiplication with carry.
  // on msvc, this optimizes identically, somehow.
  ffc_u128 z = ffc_full_multiplication(x, y);
  bool overflow;
  z.low = ffc_bigint_scalar_add(z.low, *carry, &overflow);
  z.high += (uint64_t)(overflow); // cannot overflow
  *carry = z.high;
  return z.low;
#endif
#else
  uint64_t z = (uint64_t)(x) * (uint64_t)(y) + (uint64_t)(*carry);
  *carry = (ffc_bigint_limb)(z >> FFC_LIMB_BITS);
  return (ffc_bigint_limb)(z);
#endif
}

// add scalar value to bigint starting from offset.
// used in grade school multiplication
ffc_internal ffc_inline
bool ffc_bigint_small_add_from(ffc_sv* sv, ffc_bigint_limb y, size_t start) {
  size_t index = start;
  ffc_bigint_limb carry = y;
  bool overflow;
  while (carry != 0 && index < sv->len) {
    sv->data[index] = ffc_bigint_scalar_add(sv->data[index], carry, &overflow);
    carry = (ffc_bigint_limb)(overflow);
    index += 1;
  }
  if (carry != 0) {
    FFC_TRY(ffc_sv_try_push(sv, carry));
  }
  return true;
}

// add scalar value to bigint.
ffc_internal ffc_inline
bool ffc_bigint_small_add(ffc_sv* sv, ffc_bigint_limb y) {
  return ffc_bigint_small_add_from(sv, y, 0);
}

// multiply bigint by scalar value.
ffc_internal
bool ffc_bigint_small_mul(ffc_sv* sv, ffc_bigint_limb y) {
  ffc_bigint_limb carry = 0;
  for (size_t index = 0; index < sv->len; index++) {
    sv->data[index] = ffc_bigint_scalar_mul(sv->data[index], y, &carry);
  }
  if (carry != 0) {
    FFC_TRY(ffc_sv_try_push(sv, carry));
  }
  return true;
}

// add bigint to bigint starting from index.
// used in grade school multiplication
ffc_internal
bool ffc_bigint_large_add_from(ffc_sv* x, ffc_bigint_limb_span y, size_t start) {
  // the effective x buffer is from `xstart..x.len()`, so exit early
  // if we can't get that current range.

  // FFC_DIVERGE: We are calling our try_reserve instead of the o.g. try_resize
  if (x->len < start || y.len > x->len - start) {
    FFC_TRY(ffc_sv_try_reserve(x, y.len + start));
  }

  bool carry = false;
  for (size_t index = 0; index < y.len; index++) {
    ffc_bigint_limb xi = x->data[index + start];
    ffc_bigint_limb yi = ffc_span_index(y, index);
    bool c1 = false;
    bool c2 = false;
    xi = ffc_bigint_scalar_add(xi, yi, &c1);
    if (carry) {
      xi = ffc_bigint_scalar_add(xi, 1, &c2);
    }
    x->data[index + start] = xi;
    carry = c1 | c2;
  }

  // handle overflow
  if (carry) {
    FFC_TRY(ffc_bigint_small_add_from(x, 1, y.len + start));
  }
  return true;
}

// add bigint to bigint.
ffc_internal ffc_inline
bool ffc_sv_large_add_from_zero(ffc_sv* x, ffc_bigint_limb_span y) {
  return ffc_bigint_large_add_from(x, y, 0);
}

// grade-school multiplication algorithm
ffc_internal
bool ffc_bigint_long_mul(ffc_sv* x, ffc_bigint_limb_span y) {
  ffc_bigint_limb_span xs = (ffc_bigint_limb_span){ .ptr = x->data, .len = x->len };

  // full copy of x into z
  ffc_sv z = ffc_sv_create(xs);

  ffc_bigint_limb_span zs = (ffc_bigint_limb_span){ .ptr = z.data, .len = z.len };

  if (y.len != 0) {
    ffc_bigint_limb y0 = ffc_span_index(y, 0);
    FFC_TRY(ffc_bigint_small_mul(x, y0));
    for (size_t index = 1; index < y.len; index++) {

      ffc_bigint_limb yi = ffc_span_index(y, index);
      ffc_sv zi; // re-use the same buffer throughout

      if (yi != 0) {
        zi.len = 0;
        FFC_TRY(ffc_sv_try_extend(&zi, zs));
        FFC_TRY(ffc_bigint_small_mul(&zi, yi));
        ffc_bigint_limb_span zis = (ffc_bigint_limb_span){zi.data, zi.len};
        FFC_TRY(ffc_bigint_large_add_from(x, zis, index));
      }
    }
  }

  ffc_sv_normalize(x);
  return true;
}

// grade-school multiplication algorithm
ffc_internal ffc_inline
bool ffc_bigint_large_mul(ffc_sv* x, ffc_bigint_limb_span y) {
  if (y.len == 1) {
    FFC_TRY(ffc_bigint_small_mul(x, ffc_span_index(y,0)));
  } else {
    FFC_TRY(ffc_bigint_long_mul(x, y));
  }
  return true;
}

static const uint32_t pow5_tables_large_step = 135;
static const uint64_t pow5_tables_small_powers[] = {
    1ULL,
    5ULL,
    25ULL,
    125ULL,
    625ULL,
    3125ULL,
    15625ULL,
    78125ULL,
    390625ULL,
    1953125ULL,
    9765625ULL,
    48828125ULL,
    244140625ULL,
    1220703125ULL,
    6103515625ULL,
    30517578125ULL,
    152587890625ULL,
    762939453125ULL,
    3814697265625ULL,
    19073486328125ULL,
    95367431640625ULL,
    476837158203125ULL,
    2384185791015625ULL,
    11920928955078125ULL,
    59604644775390625ULL,
    298023223876953125ULL,
    1490116119384765625ULL,
    7450580596923828125ULL,
};
#ifdef FFC_64BIT_LIMB
  static const ffc_bigint_limb ffc_large_power_of_5[] = {
      1414648277510068013ULL, 9180637584431281687ULL, 4539964771860779200ULL,
      10482974169319127550ULL, 198276706040285095ULL};
#else
  static const ffc_bigint_limb ffc_large_power_of_5[] = {
      4279965485U, 329373468U,  4020270615U, 2137533757U, 4287402176U,
      1057042919U, 1071430142U, 2440757623U, 381945767U,  46164893U};
#endif

// big integer type. implements a small subset of big integer
// arithmetic, using simple algorithms since asymptotically
// faster algorithms are slower for a small number of limbs.
// all operations assume the big-integer is normalized.
typedef struct ffc_bigint {
  // storage of the limbs, in little-endian order.
  ffc_sv vec;
} ffc_bigint;

ffc_inline ffc_internal
ffc_bigint ffc_bigint_empty(void) {
  ffc_sv sv;
  sv.len = 0;
  return (ffc_bigint){sv};
}

ffc_inline ffc_internal
ffc_bigint ffc_bigint_make(uint64_t value) {
  ffc_sv sv;
  sv.len = 0;
#ifdef FFC_64BIT_LIMB
  ffc_sv_push_unchecked(&sv, value);
#else
  ffc_sv_push_unchecked(&sv, (uint32_t)(value));
  ffc_sv_push_unchecked(&sv, (uint32_t)(value >> 32));
#endif
  ffc_sv_normalize(&sv);
  return (ffc_bigint){sv};
}

// get the high 64 bits from the vector, and if bits were truncated.
// this is to get the significant digits for the float.
ffc_inline ffc_internal
uint64_t ffc_bigint_hi64(ffc_bigint me, bool* truncated) {
  ffc_sv vec = me.vec;
#ifdef FFC_64BIT_LIMB
  if (vec.len == 0) {
    *truncated = false;
    return 0;
  } else if (vec.len == 1) {
    return ffc_uint64_hi64_1(ffc_sv_rindex(vec,0), truncated);
  } else {
    uint64_t result = ffc_uint64_hi64_2(ffc_sv_rindex(vec, 0), ffc_sv_rindex(vec, 1), truncated);
    *truncated |= ffc_sv_exists_nonzero_after(vec, 2);
    return result;
  }
#else
  if (vec.len == 0) {
    *truncated = false;
    return 0;
  } else if (vec.len == 1) {
    return ffc_uint32_hi64_1(ffc_sv_rindex(vec,0), truncated);
  } else if (vec.len == 2) {
    return ffc_uint32_hi64_2(ffc_sv_rindex(vec,0), ffc_sv_rindex(vec,1), truncated);
  } else {
    uint64_t result = ffc_uint32_hi64_3(
        ffc_sv_rindex(vec,0), 
        ffc_sv_rindex(vec,1), 
        ffc_sv_rindex(vec,2),
        truncated
    );
    *truncated |= ffc_sv_exists_nonzero_after(vec , 3);
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
ffc_internal ffc_inline 
int ffc_bigint_compare(ffc_bigint me, ffc_bigint const *other) {
  if (me.vec.len > other->vec.len) {
    return 1;
  } else if (me.vec.len < other->vec.len) {
    return -1;
  } else {
    for (size_t index = me.vec.len; index > 0; index--) {
      ffc_bigint_limb xi = ffc_sv_index(me.vec, index - 1);
      ffc_bigint_limb yi = ffc_sv_index(other->vec, index - 1);
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
ffc_internal ffc_inline 
bool ffc_bigint_shl_bits(ffc_bigint* me, size_t n) {
  // Internally, for each item, we shift left by n, and add the previous
  // right shifted limb-bits.
  // For example, we transform (for u8) shifted left 2, to:
  //      b10100100 b01000010
  //      b10 b10010001 b00001000
  FFC_DEBUG_ASSERT(n != 0);
  FFC_DEBUG_ASSERT(n < sizeof(ffc_bigint_limb) * 8);

  size_t shl = n;
  size_t shr = FFC_LIMB_BITS - shl;
  ffc_bigint_limb prev = 0;
  for (size_t index = 0; index < me->vec.len; index++) {
    ffc_bigint_limb xi = ffc_sv_index(me->vec, index);
    me->vec.data[index] = (xi << shl) | (prev >> shr);
    prev = xi;
  }

  ffc_bigint_limb carry = prev >> shr;
  if (carry != 0) {
    return ffc_sv_try_push(&me->vec, carry);
  }
  return true;
}

// move the limbs left by `n` limbs.
ffc_internal ffc_inline 
bool ffc_bigint_shl_limbs(ffc_bigint* me, size_t n) {
  FFC_DEBUG_ASSERT(n != 0);
  if (n + me->vec.len > SV_LIMB_COUNT) {
    return false;
  } else if (me->vec.len != 0) {
    // move limbs
    ffc_bigint_limb *dst = me->vec.data + n;
    ffc_bigint_limb const *src = me->vec.data;
    // std::copy_backward(src, src + vec.len(), dst + vec.len());
    // memmove to handle the overlap
    memmove(dst, src, me->vec.len * sizeof(ffc_bigint_limb));
    
    // fill in empty limbs
    ffc_bigint_limb *first = me->vec.data;
    // ffc_bigint_limb *last = first + n;
    // ::std::fill(first, last, 0);
    memset(first, 0, n * sizeof(ffc_bigint_limb));
    me->vec.len += n;
    return true;
  } else {
    return true;
  }
}

// move the limbs left by `n` bits.
ffc_internal ffc_inline 
bool ffc_bigint_shl(ffc_bigint* me, size_t n) {
  size_t rem = n % FFC_LIMB_BITS;
  size_t div = n / FFC_LIMB_BITS;
  if (rem != 0) {
    FFC_TRY(ffc_bigint_shl_bits(me, rem));
  }
  if (div != 0) {
    FFC_TRY(ffc_bigint_shl_limbs(me, div));
  }
  return true;
}

// get the number of leading zeros in the bigint.
ffc_internal ffc_inline 
int ffc_bigint_ctlz(ffc_bigint me) {
  if (me.vec.len == 0) {
    return 0;
  } else {
#ifdef FFC_64BIT_LIMB
    return (int)ffc_count_leading_zeroes(ffc_sv_rindex(me.vec, 0));
#else
    // no use defining a specialized count_leading_zeros for a 32-bit type.
    uint64_t r0 = ffc_sv_rindex(me.vec, 0);
    return ffc_count_leading_zeroes(r0 << 32);
#endif
  }
}

// get the number of bits in the bigint.
ffc_internal ffc_inline 
int ffc_bigint_bit_length(ffc_bigint me) {
  int lz = ffc_bigint_ctlz(me);
  return (int)(FFC_LIMB_BITS * me.vec.len) - lz;
}

ffc_internal ffc_inline 
bool ffc_bigint_mul(ffc_bigint* me, ffc_bigint_limb y) { return ffc_bigint_small_mul(&me->vec, y); }

ffc_internal ffc_inline 
bool ffc_bigint_add(ffc_bigint* me, ffc_bigint_limb y) { return ffc_bigint_small_add(&me->vec, y); }

// multiply as if by 2 raised to a power.
ffc_internal ffc_inline 
bool ffc_bigint_pow2(ffc_bigint* me, uint32_t exp) { return ffc_bigint_shl(me, exp); }

// multiply as if by 5 raised to a power.
ffc_internal ffc_inline 
bool ffc_bigint_pow5(ffc_bigint* me, uint32_t exp) {
  // multiply by a power of 5
  size_t large_length = sizeof(ffc_large_power_of_5) / sizeof(ffc_bigint_limb);
  ffc_bigint_limb_span large = (ffc_bigint_limb_span){ .ptr = (ffc_bigint_limb*)ffc_large_power_of_5, .len = large_length};
  while (exp >= pow5_tables_large_step) {
    FFC_TRY(ffc_bigint_large_mul(&me->vec, large));
    exp -= pow5_tables_large_step;
  }
#ifdef FFC_64BIT_LIMB
  uint32_t small_step = 27;
  ffc_bigint_limb max_native = 7450580596923828125UL;
#else
  uint32_t small_step = 13;
  ffc_bigint_limb max_native = 1220703125U;
#endif
  while (exp >= small_step) {
    FFC_TRY(ffc_bigint_small_mul(&me->vec, max_native));
    exp -= small_step;
  }
  if (exp != 0) {
    FFC_TRY(
      ffc_bigint_small_mul(&me->vec, (ffc_bigint_limb)(pow5_tables_small_powers[exp]))
    );
  }

  return true;
}

// multiply as if by 10 raised to a power.
ffc_internal ffc_inline 
bool ffc_bigint_pow10(ffc_bigint* me, uint32_t exp) {
  FFC_TRY(ffc_bigint_pow5(me, exp));
  return ffc_bigint_pow2(me, exp);
}

#undef ffc_span_index
#undef sv_rindex

#endif // FFC_BIGINT_H
