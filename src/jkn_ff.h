/* jkn_ff.h
   single-header decimal float parser using eisel-lemire
   Usage:
       #define JKN_FF_IMPL
       #include "jkn_ff.h"
  
       jkn_ff_parse_double(first, last, &out)
       jkn_ff_parse_float(first, last, &out)

       nocommit licenses, contributors, and amalgamation
*/

#ifndef JKN_FF_H
#define JKN_FF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdlib.h> // nocommit for exit
#include <stdio.h> // nocommit for printf

typedef enum jkn_ff_outcome {
  jkn_ff_outcome_ok = 0,
  jkn_ff_outcome_invalid_input = 1,
  jkn_ff_outcome_out_of_range = 2,
} jkn_ff_outcome;

typedef struct jkn_ff_result {
  // Where parsing stopped
  char *ptr;
  // The outcome of the call
  jkn_ff_outcome outcome;
} jkn_ff_result;

// nocommit: restrict since its char*?
jkn_ff_result jkn_ff_parse_double(size_t len, const char input[len], double* out);
jkn_ff_result jkn_ff_parse_float(size_t len, const char input[len], float* out);

#ifdef JKN_FF_IMPL

#include "common.h"
#include <math.h>

#include "parse.h"

/* section: decimal to binary */

jkn_ff_inline jkn_ff_internal
jkn_ff_u128 jkn_ff_compute_product_approximation_double(int64_t q, uint64_t w) {
  // The required precision is mantissa_explicit_bits + 3 because
  // 1. We need the implicit bit
  // 2. We need an extra bit for rounding purposes
  // 3. We might lose a bit due to the "upperbit" routine (result too small,
  // requiring a shift)
  uint64_t bit_precision = DOUBLE_MANTISSA_EXPLICIT_BITS + 3;
  uint64_t precision_mask = ((uint64_t)(0xFFFFFFFFFFFFFFFF) >> bit_precision);

  int const index = 2 * (int)(q - DOUBLE_SMALLEST_POWER_OF_10);

  // For small values of q, e.g., q in [0,27], the answer is always exact
  // because jkn_ff_mul_u64(w, powers_of_five[index]) gives the exact answer.
  jkn_ff_u128 firstproduct = jkn_ff_mul_u64(w, jkn_ff_powers_of_five[index]);
                           
  if ((firstproduct.high & precision_mask) == precision_mask) {
    // could further guard with  (lower + w < lower)
    // regarding the second product, we only need secondproduct.hi, but our
    // expectation is that the compiler will optimize this extra work away if
    // needed.
    jkn_ff_u128 secondproduct = jkn_ff_mul_u64(w, jkn_ff_powers_of_five[index + 1]);

    firstproduct.low += secondproduct.high;
    if (secondproduct.high > firstproduct.low) {
      firstproduct.high++;
    }
  }
  return firstproduct;
}

/**
 * For q in (0,350), we have that
 *  f = (((152170 + 65536) * q ) >> 16);
 * is equal to
 *   floor(p) + q
 * where
 *   p = log(5**q)/log(2) = q * log(5)/log(2)
 *
 * For negative values of q in (-400,0), we have that
 *  f = (((152170 + 65536) * q ) >> 16);
 * is equal to
 *   -ceil(p) + q
 * where
 *   p = log(5**-q)/log(2) = -q * log(5)/log(2)

 * FF_DIVERGE: renamed from detail::power to b10_to_b2
 */
jkn_ff_internal jkn_ff_inline int32_t jkn_ff_b10_to_b2(int32_t q) {
  return (((152170 + 65536) * q) >> 16) + 63;
}

// Computes w * 10 ** q.
// The returned value should be a valid number that simply needs to be
// packed. However, in some very rare cases, the computation will fail. In such
// cases, we return an adjusted_mantissa with a negative power of 2: the caller
// should recompute in such cases.
jkn_ff_inline jkn_ff_internal
jkn_ff_adjusted_mantissa jkn_ff_compute_float_double(int64_t q, uint64_t w) {
  jkn_ff_adjusted_mantissa answer;
  if ((w == 0) || (q < DOUBLE_SMALLEST_POWER_OF_10)) {
    answer.power2 = 0;
    answer.mantissa = 0;
    // result should be zero
    return answer;
  }
  if (q > DOUBLE_LARGEST_POWER_OF_10) {
    // we want to get infinity:
    answer.power2 = DOUBLE_INFINITE_POWER;
    answer.mantissa = 0;
    return answer;
  }
  // At this point in time q is in [powers::smallest_power_of_five,
  // powers::largest_power_of_five].

  // We want the most significant bit of i to be 1. Shift if needed.
  int lz = jkn_ff_count_leading_zeroes(w);
  w <<= lz;

  jkn_ff_u128 product = jkn_ff_compute_product_approximation_double(q, w);

  // The computed 'product' is always sufficient.
  // Mathematical proof:
  // Noble Mushtak and Daniel Lemire, Fast Number Parsing Without Fallback (to
  // appear) See script/mushtak_lemire.py

  // The "compute_product_approximation" function can be slightly slower than a
  // branchless approach: value128 product = compute_product(q, w); but in
  // practice, we can win big with the compute_product_approximation if its
  // additional branch is easily predicted. Which is data specific.
  int upperbit = (int)(product.high >> 63);
  int shift = upperbit + 64 - DOUBLE_MANTISSA_EXPLICIT_BITS - 3;

  answer.mantissa = product.high >> shift;

  // compute a biased-up power of 2
  // - the upperbit thing (? nocommit)
  // - leading zeroes in the input mantissa detract from the exponent
  answer.power2 = (int32_t)(jkn_ff_b10_to_b2((int32_t)(q)) + upperbit - lz - DOUBLE_MINIMUM_EXPONENT);

  if (answer.power2 <= 0) { // subnormal path

    if (-answer.power2 + 1 >= 64) {
      // if we have more than 64 bits below the minimum exponent, you
      // have a zero for sure.
      answer.power2 = 0;
      answer.mantissa = 0;
      // result should be zero
      return answer;
    }
    // shift is safe because -answer.power2 + 1 < 64
    answer.mantissa >>= -answer.power2 + 1;

    // Thankfully, we can't have both "round-to-even" and subnormals because
    // "round-to-even" only occurs for powers close to 0 in the 32-bit and
    // and 64-bit case (with no more than 19 digits), so we round up.
    answer.mantissa += (answer.mantissa & 1); // round up
    answer.mantissa >>= 1;

    // weird scenario:
    // Suppose we start with 2.2250738585072013e-308, we end up
    // with 0x3fffffffffffff x 2^-1023-53 which is technically subnormal
    // whereas 0x40000000000000 x 2^-1023-53  is normal. Now, we need to round
    // up 0x3fffffffffffff x 2^-1023-53  and once we do, we are no longer
    // subnormal, but we can only know this after rounding.
    // So we only declare a subnormal if we are smaller than the threshold.
    answer.power2 =
      (answer.mantissa < ((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS)) ? 0 : 1;
    return answer;
  } // subnormal

  // usually, we round *up*, but if we fall right in between and we have an
  // even basis, we need to round down
  // We are only concerned with the cases where 5**q fits in single 64-bit word.

  // 'extremely sparse low bits in our product' and
  // 'q is within the round to even range' and
  // 'mantissa lowest 2 bits are exactly 01'
  if ((product.low <= 1) && (q >= DOUBLE_MIN_EXPONENT_ROUND_TO_EVEN) &&
      (q <= DOUBLE_MAX_EXPONENT_ROUND_TO_EVEN) &&
      ((answer.mantissa & 3) == 1)) { // we may fall between two floats!
                                      //
    // To be in-between two floats we need that in doing
    //   answer.mantissa = product.high >> (upperbit + 64 -
    //   binary::mantissa_explicit_bits() - 3);
    // ... we dropped out only zeroes. But if this happened, then we can go
    // back!!!

    // mask off last bit
    if ((answer.mantissa << shift) == product.high) {
      answer.mantissa &= ~(uint64_t)(1);
    }
  }

  answer.mantissa += (answer.mantissa & 1); // round up
  answer.mantissa >>= 1;
  // nocommit: What's this check?
  if (answer.mantissa >= ((uint64_t)(2) << DOUBLE_MANTISSA_EXPLICIT_BITS)) {
    answer.mantissa = ((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS);
    answer.power2++; // undo previous addition
  }

  // normalize to pos INF?
  answer.mantissa &= ~((uint64_t)(1) << DOUBLE_MANTISSA_EXPLICIT_BITS);
  if (answer.power2 >= DOUBLE_INFINITE_POWER) { // infinity
    answer.power2 = DOUBLE_INFINITE_POWER;
    answer.mantissa = 0;
  }
  return answer;
}

// create an adjusted mantissa, biased by the invalid power2
// for significant digits already multiplied by 10 ** q.
jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_compute_error_scaled_double(int64_t q, uint64_t w, int lz) {
  int hilz = (int)(w >> 63) ^ 1;
  jkn_ff_adjusted_mantissa answer;
  answer.mantissa = w << hilz;
  int bias = DOUBLE_MANTISSA_EXPLICIT_BITS - DOUBLE_MINIMUM_EXPONENT;
  answer.power2 = (int32_t)(jkn_ff_b10_to_b2((int32_t)(q)) + bias - hilz - lz - 62 +
                          JKN_FF_INVALID_AM_BIAS);
  return answer;
}

// w * 10 ** q, without rounding the representation up.
// the power2 in the exponent will be adjusted by invalid_am_bias.
jkn_ff_internal jkn_ff_inline
jkn_ff_adjusted_mantissa jkn_ff_compute_error_double(int64_t q, uint64_t w) {
  int lz = jkn_ff_count_leading_zeroes(w);
  w <<= lz;
  jkn_ff_u128 product = jkn_ff_compute_product_approximation_double(q, w);
  return jkn_ff_compute_error_scaled_double(q, product.high, lz);
}

/* end section: decimal to binary */

/* section: entrypoint */

jkn_ff_internal jkn_ff_inline
bool jkn_ff_clinger_fast_path_impl_double(uint64_t mantissa, int64_t exponent, bool is_negative,
                       double* value) {
  // The implementation of the Clinger's fast path is convoluted because
  // we want round-to-nearest in all cases, irrespective of the rounding mode
  // selected on the thread.
  // We proceed optimistically, assuming that detail::rounds_to_nearest()
  // returns true.
  if (DOUBLE_MIN_EXPONENT_FAST_PATH <= exponent &&
      exponent <= DOUBLE_MAX_EXPONENT_FAST_PATH) {
    // Unfortunately, the conventional Clinger's fast path is only possible
    // when the system rounds to the nearest float.
    //
    // We expect the next branch to almost always be selected.
    // We could check it first (before the previous branch), but
    // there might be performance advantages at having the check
    // be last.
    if (jkn_ff_rounds_to_nearest()) {
      // We have that fegetround() == FE_TONEAREST.
      // Next is Clinger's fast path.
      if (mantissa <= DOUBLE_MAX_MANTISSA_FAST_PATH) {
        *value = (double)(mantissa);
        if (exponent < 0) {
          *value = *value / double_powers_of_ten[-exponent];
        } else {
          *value = *value * double_powers_of_ten[exponent];
        }
        if (is_negative) {
          *value = -*value;
        }
        return true;
      }
    } else {
      // We do not have that fegetround() == FE_TONEAREST.
      // Next is a modified Clinger's fast path, inspired by Jakub Jelínek's
      // proposal
      if (exponent >= 0 &&
          mantissa <= double_max_mantissa[exponent]) {
#if defined(__clang__) || defined(FASTFLOAT_32BIT)
        // Clang may map 0 to -0.0 when fegetround() == FE_DOWNWARD
        if (mantissa == 0) {
          *value = is_negative ? -0. : 0.;
          return true;
        }
#endif
        *value = (double)mantissa * double_powers_of_ten[exponent];
        if (is_negative) {
          *value = -*value;
        }
        return true;
      }
    }
  }
  return false;
}

jkn_ff_internal jkn_ff_inline
jkn_ff_result jkn_ff_from_chars_advanced_double(jkn_ff_parsed const pns, double* value) {

  jkn_ff_result answer;

  answer.outcome = jkn_ff_outcome_ok; // be optimistic :')
  answer.ptr = (char*)pns.lastmatch;

  if (!pns.too_many_digits &&
      jkn_ff_clinger_fast_path_impl_double(pns.mantissa, pns.exponent, pns.negative, value))
    return answer;

  jkn_ff_adjusted_mantissa am =
      jkn_ff_compute_float_double(pns.exponent, pns.mantissa);
  jkn_ff_debug("am.mantissa: %llu\n", am.mantissa);
  jkn_ff_debug("am.power2:   %d\n", am.power2);
  if (pns.too_many_digits && am.power2 >= 0) {
    jkn_ff_adjusted_mantissa am_plus_one = jkn_ff_compute_float_double(pns.exponent, pns.mantissa + 1);
    bool equal = am.mantissa == am_plus_one.mantissa && am.power2 == am_plus_one.power2;
    if (!equal) {
      am = jkn_ff_compute_error_double(pns.exponent, pns.mantissa);
    }
  }
  // If we called compute_float<binary_format<T>>(pns.exponent, pns.mantissa)
  // and we have an invalid power (am.power2 < 0), then we need to go the long
  // way around again. This is very uncommon.
  if (am.power2 < 0) {
    printf("digit_comp unimplemented");
    exit(1);
    // am = digit_comp<T>(pns, am);
  }
  jkn_ff_am_to_float_double(pns.negative, am, value);
  jkn_ff_debug("value: %f\n", *value);
  // Test for over/underflow.
  if ((pns.mantissa != 0 && am.mantissa == 0 && am.power2 == 0) ||
      am.power2 == DOUBLE_INFINITE_POWER) {
    answer.outcome = jkn_ff_outcome_out_of_range;
  }
  return answer;
}

/**
 * Special case +inf, -inf, nan, infinity, -infinity.
 * The case comparisons could be made much faster given that we know that the
 * strings a null-free and fixed.
 **/
jkn_ff_internal jkn_ff_inline
jkn_ff_result jkn_ff_parse_infnan(
    char *first, char *last,
    double* value, jkn_ff_chars_format fmt
) {
  jkn_ff_result answer;
  // nocommit: is there a better way to discard the const qualifier?
  answer.ptr = first;
  answer.outcome = jkn_ff_outcome_ok; // be optimistic
  // assume first < last, so dereference without checks;
  bool const minusSign = (*first == '-');
  // C++17 20.19.3.(7.1) explicitly forbids '+' sign here
  if ((*first == '-') ||
      ((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_ALLOW_LEADING_PLUS) &&
       (*first == '+'))) {
    ++first;
  }
  if (last - first >= 3) {
    if (fastfloat_strncasecmp3(first, "nan", 1)) {
      answer.ptr = (first += 3);
      *value = minusSign ? -(double)NAN : (double)NAN;
      // Check for possible nan(n-char-seq-opt), C++17 20.19.3.7,
      // C11 7.20.1.3.3. At least MSVC produces nan(ind) and nan(snan).
      if (first != last && *first == '(') {
        for (char *ptr = first + 1; ptr != last; ++ptr) {
          if (*ptr == ')') {
            answer.ptr = ptr + 1; // valid nan(n-char-seq-opt)
            break;
          } else if (!(('a' <= *ptr && *ptr <= 'z') ||
                       ('A' <= *ptr && *ptr <= 'Z') ||
                       ('0' <= *ptr && *ptr <= '9') || *ptr == '_'))
            break; // forbidden char, not nan(n-char-seq-opt)
        }
      }
      return answer;
    }
    if (fastfloat_strncasecmp3(first, "infinity", 1)) {
      if ((last - first >= 8) &&
          fastfloat_strncasecmp5(first + 3, (char*)&"infinity"[3], 1)) {
        answer.ptr = first + 8;
      } else {
        answer.ptr = first + 3;
      }
      *value = minusSign ? -(double)NAN : (double)NAN;
      return answer;
    }
  }
  answer.outcome = jkn_ff_outcome_invalid_input;
  return answer;
}

// internal entrypoint
jkn_ff_internal jkn_ff_inline
jkn_ff_result from_chars_double(char* first, char* last, double* value, jkn_ff_parse_options options) {

  // Alias for parity with cpp code, no feature macros to apply
  jkn_ff_chars_format const fmt = options.format;

  jkn_ff_result answer;
  if ((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_SKIP_WHITE_SPACE)) {
    while ((first != last) && jkn_ff_is_space(*first)) {
      first++;
    }
  }
  if (first == last) {
    answer.outcome = jkn_ff_outcome_invalid_input;
    answer.ptr = first;
    return answer;
  }
  jkn_ff_parsed pns =
      (uint64_t)(fmt & JKN_FF_FORMAT_FLAG_BASIC_JSON)
          ? jkn_ff_parse_number_string(first, last, options, true)
          : jkn_ff_parse_number_string(first, last, options, false);

  #ifdef JKN_FF_DEBUG
  jkn_ff_dump_parsed(pns);
  #endif                          

  if (!pns.valid) {
    if ((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_NO_INFNAN)) {
      answer.outcome = jkn_ff_outcome_invalid_input;
      answer.ptr = first;
      return answer;
    } else {
      return jkn_ff_parse_infnan(first, last, value, fmt);
    }
  }

  // call overload that takes parsed_number_string_t directly.
  return jkn_ff_from_chars_advanced_double(pns, value);
}

#undef DOUBLE_SMALLEST_POWER_OF_10       
#undef DOUBLE_LARGEST_POWER_OF_10        
#undef DOUBLE_SIGN_INDEX
#undef DOUBLE_INFINITE_POWER             
#undef DOUBLE_MANTISSA_EXPLICIT_BITS     
#undef DOUBLE_MINIMUM_EXPONENT           
#undef DOUBLE_MIN_EXPONENT_ROUND_TO_EVEN 
#undef DOUBLE_MAX_EXPONENT_ROUND_TO_EVEN 

#undef FLOAT_SMALLEST_POWER_OF_10        
#undef FLOAT_LARGEST_POWER_OF_10         
#undef FLOAT_SIGN_INDEX
#undef FLOAT_INFINITE_POWER              
#undef FLOAT_MANTISSA_EXPLICIT_BITS      
#undef FLOAT_MINIMUM_EXPONENT            
#undef FLOAT_MIN_EXPONENT_ROUND_TO_EVEN  
#undef FLOAT_MAX_EXPONENT_ROUND_TO_EVEN  

#endif /* JKN_FF_IMPL */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* JKN_FF_H */

