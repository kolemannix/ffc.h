#ifndef FASTFLOAT_DIGIT_COMPARISON_H
#define FASTFLOAT_DIGIT_COMPARISON_H

#include "common.h"
#include "bigint.h"
#include "parse.h"

// 1e0 to 1e19
static const uint64_t jkn_ff_powers_of_ten_uint64[] = {1UL,
                                                    10UL,
                                                    100UL,
                                                    1000UL,
                                                    10000UL,
                                                    100000UL,
                                                    1000000UL,
                                                    10000000UL,
                                                    100000000UL,
                                                    1000000000UL,
                                                    10000000000UL,
                                                    100000000000UL,
                                                    1000000000000UL,
                                                    10000000000000UL,
                                                    100000000000000UL,
                                                    1000000000000000UL,
                                                    10000000000000000UL,
                                                    100000000000000000UL,
                                                    1000000000000000000UL,
                                                    10000000000000000000UL};

// calculate the exponent, in scientific notation, of the number.
// this algorithm is not even close to optimized, but it has no practical
// effect on performance: in order to have a faster algorithm, we'd need
// to slow down performance for faster algorithms, and this is still fast.
jkn_ff_inline jkn_ff_internal int32_t
scientific_exponent(uint64_t mantissa, int32_t exponent) {
  while (mantissa >= 10000) {
    mantissa /= 10000;
    exponent += 4;
  }
  while (mantissa >= 100) {
    mantissa /= 100;
    exponent += 2;
  }
  while (mantissa >= 10) {
    mantissa /= 10;
    exponent += 1;
  }
  return exponent;
}

// this converts a native floating-point number to an extended-precision float.
jkn_ff_internal jkn_ff_inline jkn_ff_adjusted_mantissa
jkn_ff_to_extended_double(double value) {
  uint64_t const exponent_mask = DOUBLE_EXPONENT_MASK;
  uint64_t const mantissa_mask = DOUBLE_MANTISSA_MASK;
  uint64_t const hidden_bit_mask = DOUBLE_HIDDEN_BIT_MASK;

  jkn_ff_adjusted_mantissa am;
  int32_t bias = DOUBLE_MANTISSA_EXPLICIT_BITS - DOUBLE_MINIMUM_EXPONENT;
  uint64_t bits;
  memcpy(&bits, &value, sizeof(double));

  if ((bits & exponent_mask) == 0) {
    // denormal
    am.power2 = 1 - bias;
    am.mantissa = bits & mantissa_mask;
  } else {
    // normal
    am.power2 = (int32_t)((bits & exponent_mask) >> DOUBLE_MANTISSA_EXPLICIT_BITS);
    am.power2 -= bias;
    am.mantissa = (bits & mantissa_mask) | hidden_bit_mask;
  }

  return am;
}

jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_to_extended_float(float value) {
  uint32_t const exponent_mask = FLOAT_EXPONENT_MASK;
  uint32_t const mantissa_mask = FLOAT_MANTISSA_MASK;
  uint32_t const hidden_bit_mask = FLOAT_HIDDEN_BIT_MASK;

  jkn_ff_adjusted_mantissa am;
  int32_t bias = FLOAT_MANTISSA_EXPLICIT_BITS - FLOAT_MINIMUM_EXPONENT;
  uint32_t bits;
  memcpy(&bits, &value, sizeof(float));

  if ((bits & exponent_mask) == 0) {
    // denormal
    am.power2 = 1 - bias;
    am.mantissa = bits & mantissa_mask;
  } else {
    // normal
    am.power2 = (int32_t)((bits & exponent_mask) >> FLOAT_MANTISSA_EXPLICIT_BITS);
    am.power2 -= bias;
    am.mantissa = (bits & mantissa_mask) | hidden_bit_mask;
  }

  return am;
}

// get the extended precision value of the halfway point between b and b+u.
// we are given a native float that represents b, so we need to adjust it
// halfway between b and b+u.
jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_to_extended_halfway_double(double value) {
  jkn_ff_adjusted_mantissa am = jkn_ff_to_extended_double(value);
  am.mantissa <<= 1;
  am.mantissa += 1;
  am.power2 -= 1;
  return am;
}

jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_to_extended_halfway_float(float value) {
  jkn_ff_adjusted_mantissa am = jkn_ff_to_extended_float(value);
  am.mantissa <<= 1;
  am.mantissa += 1;
  am.power2 -= 1;
  return am;
}

// round an extended-precision float to the nearest machine float.
jkn_ff_internal jkn_ff_inline
void jkn_ff_round_double(jkn_ff_adjusted_mantissa *am, callback cb) {
  int32_t mantissa_shift = 64 - DOUBLE_MANTISSA_EXPLICIT_BITS - 1;
  if (-am->power2 >= mantissa_shift) {
    // have a denormal float
    int32_t shift = -am->power2 + 1;
    cb(am, MIN(shift, 64));
    // check for round-up: if rounding-nearest carried us to the hidden bit.
    am->power2 = (am->mantissa < ((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS))
                    ? 0
                    : 1;
    return;
  }

  // have a normal float, use the default shift.
  cb(am, mantissa_shift);

  // check for carry
  if (am->mantissa >= ((uint64_t)(2) << DOUBLE_MANTISSA_EXPLICIT_BITS)) {
    am->mantissa = ((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS);
    am->power2++;
  }

  // check for infinite: we could have carried to an infinite power
  am->mantissa &= ~((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS);
  if (am->power2 >= DOUBLE_INFINITE_POWER) {
    am->power2 = DOUBLE_INFINITE_POWER;
    am->mantissa = 0;
  }
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_round_nearest_tie_even(jkn_ff_adjusted_mantissa *am, int32_t shift, callback cb) {
  uint64_t const mask = (shift == 64) ? UINT64_MAX : ((uint64_t)(1) << shift) - 1;
  uint64_t const halfway = (shift == 0) ? 0 : (uint64_t)(1) << (shift - 1);
  uint64_t truncated_bits = am->mantissa & mask;
  bool is_above = truncated_bits > halfway;
  bool is_halfway = truncated_bits == halfway;

  // shift digits into position
  if (shift == 64) {
    am->mantissa = 0;
  } else {
    am->mantissa >>= shift;
  }
  am->power2 += shift;

  bool is_odd = (am->mantissa & 1) == 1;
  am->mantissa += (uint64_t)(cb(is_odd, is_halfway, is_above));
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_round_down(jkn_ff_adjusted_mantissa* am, int32_t shift) {
  if (shift == 64) {
    am->mantissa = 0;
  } else {
    am->mantissa >>= shift;
  }
  am->power2 += shift;
}

/* 1-byte chars (char, uint8_t) */
#define JKN_FF_INT_CMP_ZEROS_1 0x3030303030303030ULL
#define JKN_FF_INT_CMP_LEN_1   8

/* 2-byte chars (uint16_t, wchar_t on Windows) */
#define JKN_FF_INT_CMP_ZEROS_2 0x0030003000300030ULL
#define JKN_FF_INT_CMP_LEN_2   4

/* 4-byte chars (uint32_t, wchar_t on Linux) */
#define JKN_FF_INT_CMP_ZEROS_4 0x0000003000000030ULL
#define JKN_FF_INT_CMP_LEN_4   2

jkn_ff_internal jkn_ff_inline
bool jkn_ff_char_eq_zero(char const *p, size_t char_width) {
  switch (char_width) {
    case 1: return *p == '0';
    case 2: return *(uint16_t const *)p == 0x0030;
    case 4: return *(uint32_t const *)p == 0x00000030;
    default: return false;
  }
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_skip_zeros(char const **first, char const *last, size_t char_width) {
  size_t cmp_len;
  size_t cmp_mask;
  switch (char_width) {
    case 1:
      cmp_len = JKN_FF_INT_CMP_LEN_1;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_1;
      break;
    case 2:
      cmp_len = JKN_FF_INT_CMP_LEN_2;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_2;
      break;
    case 4:
      cmp_len = JKN_FF_INT_CMP_LEN_4;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_4;
      break;
    default:
      JKN_FF_DEBUG_ASSERT(0);
      return;
  }

  uint64_t val;
  while (last - *first >= cmp_len) {
    memcpy(&val, *first, sizeof(uint64_t));
    if (val != cmp_mask) {
      break;
    }
    *first += cmp_len;
  }
  while (*first != last) {
    if (jkn_ff_char_eq_zero(*first, char_width)) {
      break;
    }
    *first += 1;
  }
}

// determine if any non-zero digits were truncated.
// all characters must be valid digits.
jkn_ff_internal jkn_ff_inline
bool is_truncated(char const *first, char const *last, size_t char_width) {
  size_t cmp_len;
  size_t cmp_mask;
  switch (char_width) {
    case 1:
      cmp_len = JKN_FF_INT_CMP_LEN_1;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_1;
      break;
    case 2:
      cmp_len = JKN_FF_INT_CMP_LEN_2;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_2;
      break;
    case 4:
      cmp_len = JKN_FF_INT_CMP_LEN_4;
      cmp_mask = JKN_FF_INT_CMP_ZEROS_4;
      break;
    default:
      JKN_FF_DEBUG_ASSERT(0);
      return 0;
  }
  // do 8-bit optimizations, can just compare to 8 literal 0s.
  uint64_t val;
  while (last - first >= cmp_len) {
    memcpy(&val, first, sizeof(uint64_t));
    if (val != cmp_mask) {
      return true;
    }
    first += cmp_len;
  }
  while (first != last) {
    if (jkn_ff_char_eq_zero(first, char_width)) {
      return true;
    }
    ++first;
  }
  return false;
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_parse_eight_digits(char const **p, limb *value, size_t *counter, size_t *count) {
  *value = *value * 100000000 + jkn_ff_parse_eight_digits_unrolled(*p);
  *p += 8;
  *counter += 8;
  *count += 8;
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_parse_one_digit(char const **p, limb *value, size_t *counter, size_t *count) {
  *value = *value * 10 + (limb)(**p - '0');
  p++;
  counter++;
  count++;
}

jkn_ff_internal jkn_ff_inline
void jkn_ff_add_native(jkn_ff_bigint *big, limb power, limb value) {
  jkn_ff_bigint_mul(big, power);
  jkn_ff_bigint_add(big, value);
}

jkn_ff_internal jkn_ff_inline
void round_up_bigint(jkn_ff_bigint *big, size_t *count) {
  // need to round-up the digits, but need to avoid rounding
  // ....9999 to ...10000, which could cause a false halfway point.
  jkn_ff_add_native(big, 10, 1);
  count++;
}

// parse the significant digits into a big integer
jkn_ff_internal jkn_ff_inline
void jkn_ff_parse_mantissa(jkn_ff_bigint *result, jkn_ff_parsed *num,
               size_t max_digits, size_t *digits) {
  // try to minimize the number of big integer and scalar multiplication.
  // therefore, try to parse 8 digits at a time, and multiply by the largest
  // scalar value (9 or 19 digits) for each step.
  size_t counter = 0;
  digits = 0;
  limb value = 0;
#ifdef FASTFLOAT_64BIT_LIMB
  size_t step = 19;
#else
  size_t step = 9;
#endif

  // process all integer digits.
  char const *p = num->int_part_start;
  char const *pend = p + num->int_part_len;
  jkn_ff_skip_zeros(&p, pend, 1);
  // process all digits, in increments of step per loop
  while (p != pend) {
    while ((pend - p >= 8) && (step - counter >= 8) &&
           (max_digits - *digits >= 8)) {
      jkn_ff_parse_eight_digits(&p, &value, &counter, digits);
    }
    while (counter < step && p != pend && *digits < max_digits) {
      jkn_ff_parse_one_digit(&p, &value, &counter, digits);
    }
    if (*digits == max_digits) {
      // add the temporary value, then check if we've truncated any digits
      jkn_ff_add_native(result, (limb)(jkn_ff_powers_of_ten_uint64[counter]), value);
      bool truncated = is_truncated(p, pend, 1);
      if (num->fraction_part_start != NULL) {
        truncated |= is_truncated(num->fraction_part_start, num->fraction_part_start + num->fraction_part_len, 1);
      }
      if (truncated) {
        round_up_bigint(result, digits);
      }
      return;
    } else {
      jkn_ff_add_native(result, (limb)(jkn_ff_powers_of_ten_uint64[counter]), value);
      counter = 0;
      value = 0;
    }
  }

  // add our fraction digits, if they're available.
  if (num->fraction_part_start != NULL) {
    p = num->fraction_part_start;
    pend = p + num->fraction_part_len;
    if (digits == 0) {
      jkn_ff_skip_zeros(&p, pend, 1);
    }
    // process all digits, in increments of step per loop
    while (p != pend) {
      while ((pend - p >= 8) && (step - counter >= 8) &&
             (max_digits - *digits >= 8)) {
        jkn_ff_parse_eight_digits(&p, &value, &counter, digits);
      }
      while (counter < step && p != pend && *digits < max_digits) {
        jkn_ff_parse_one_digit(&p, &value, &counter, digits);
      }
      if (*digits == max_digits) {
        // add the temporary value, then check if we've truncated any digits
        jkn_ff_add_native(result, (limb)(jkn_ff_powers_of_ten_uint64[counter]), value);
        bool truncated = is_truncated(p, pend, 1);
        if (truncated) {
          round_up_bigint(result, digits);
        }
        return;
      } else {
        jkn_ff_add_native(result, (limb)(jkn_ff_powers_of_ten_uint64[counter]), value);
        counter = 0;
        value = 0;
      }
    }
  }

  if (counter != 0) {
    jkn_ff_add_native(result, (limb)(jkn_ff_powers_of_ten_uint64[counter]), value);
  }
}

jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_positive_digit_comp_double(jkn_ff_bigint *bigmant, int32_t exponent) {
  JKN_FF_ASSERT(jkn_ff_bigint_pow10(bigmant, (uint32_t)(exponent)));
  jkn_ff_adjusted_mantissa answer;
  bool truncated;
  answer.mantissa = jkn_ff_bigint_hi64(*bigmant, &truncated);
  int bias = DOUBLE_MANTISSA_EXPLICIT_BITS - DOUBLE_MINIMUM_EXPONENT;
  answer.power2 = jkn_ff_bigint_bit_length(*bigmant) - 64 + bias;

  jkn_ff_round_double(answer, [truncated](adjusted_mantissa &a, int32_t shift) {
    round_nearest_tie_even(
        a, shift,
        [truncated](bool is_odd, bool is_halfway, bool is_above) -> bool {
          return is_above || (is_halfway && truncated) ||
                 (is_odd && is_halfway);
        });
  });

  return answer;
}

// the scaling here is quite simple: we have, for the real digits `m * 10^e`,
// and for the theoretical digits `n * 2^f`. Since `e` is always negative,
// to scale them identically, we do `n * 2^f * 5^-f`, so we now have `m * 2^e`.
// we then need to scale by `2^(f- e)`, and then the two significant digits
// are of the same magnitude.
jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_negative_digit_comp_double(
    jkn_ff_bigint *bigmant, jkn_ff_adjusted_mantissa am, int32_t exponent) {
  jkn_ff_bigint *real_digits = bigmant;
  int32_t real_exp = exponent;

  // get the value of `b`, rounded down, and get a bigint representation of b+h
  jkn_ff_adjusted_mantissa am_b = am;
  // gcc7 buf: use a lambda to remove the noexcept qualifier bug with
  // -Wnoexcept-type.
  jkn_ff_round_double(am_b,
           [](adjusted_mantissa &a, int32_t shift) { round_down(a, shift); });
  double b;
  jkn_ff_am_to_float_double(false, am_b, &b);
  jkn_ff_adjusted_mantissa theor = jkn_ff_to_extended_halfway_double(b);
  jkn_ff_bigint theor_digits = jkn_ff_bigint_make(theor.mantissa);
  int32_t theor_exp = theor.power2;

  // scale real digits and theor digits to be same power.
  int32_t pow2_exp = theor_exp - real_exp;
  uint32_t pow5_exp = (uint32_t)(-real_exp);
  if (pow5_exp != 0) {
    JKN_FF_ASSERT(jkn_ff_bigint_pow5(&theor_digits, pow5_exp));
  }
  if (pow2_exp > 0) {
    JKN_FF_ASSERT(jkn_ff_bigint_pow2(&theor_digits, (uint32_t)(pow2_exp)));
  } else if (pow2_exp < 0) {
    JKN_FF_ASSERT(jkn_ff_bigint_pow2(real_digits,(uint32_t)(-pow2_exp)));
  }

  // compare digits, and use it to direct rounding
  int ord = jkn_ff_bigint_compare(*real_digits, &theor_digits);
  jkn_ff_adjusted_mantissa answer = am;
  jkn_ff_round_double(answer, [ord](adjusted_mantissa &a, int32_t shift) {
    round_nearest_tie_even(
        a, shift, [ord](bool is_odd, bool _, bool __) -> bool {
          (void)_;  // not needed, since we've done our comparison
          (void)__; // not needed, since we've done our comparison
          if (ord > 0) {
            return true;
          } else if (ord < 0) {
            return false;
          } else {
            return is_odd;
          }
        });
  });

  return answer;
}

// parse the significant digits as a big integer to unambiguously round
// the significant digits. here, we are trying to determine how to round
// an extended float representation close to `b+h`, halfway between `b`
// (the float rounded-down) and `b+u`, the next positive float. this
// algorithm is always correct, and uses one of two approaches. when
// the exponent is positive relative to the significant digits (such as
// 1234), we create a big-integer representation, get the high 64-bits,
// determine if any lower bits are truncated, and use that to direct
// rounding. in case of a negative exponent relative to the significant
// digits (such as 1.2345), we create a theoretical representation of
// `b` as a big-integer type, scaled to the same binary exponent as
// the actual digits. we then compare the big integer representations
// of both, and use that to direct rounding.
jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa digit_comp(jkn_ff_parsed *num, jkn_ff_adjusted_mantissa am) {
  // remove the invalid exponent bias
  am.power2 -= JKN_FF_INVALID_AM_BIAS;

  int32_t sci_exp =
      scientific_exponent(num->mantissa, (int32_t)(num->exponent));
  size_t max_digits = DOUBLE_MAX_DIGITS;
  size_t digits = 0;
  jkn_ff_bigint bigmant;
  jkn_ff_parse_mantissa(&bigmant, num, max_digits, &digits);
  // can't underflow, since digits is at most max_digits.
  int32_t exponent = sci_exp + 1 - (int32_t)(digits);
  if (exponent >= 0) {
    return jkn_ff_positive_digit_comp_double(&bigmant, exponent);
  } else {
    return jkn_ff_negative_digit_comp_double(&bigmant, am, exponent);
  }
}

#endif // FASTFLOAT_DIGIT_COMPARISON_H
