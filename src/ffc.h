/* ffc.h
   single-header decimal float parser using eisel-lemire
   This is a direct port by Koleman Nix of the fast_float library
   authored by Daniel Lemire
*/

#ifndef FFC_H
#define FFC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "api.h"

#ifdef FFC_IMPL

#include "common.h"

#include "parse.h"
#include "digit_comparison.h"

/* section: decimal to binary */

ffc_inline ffc_internal
ffc_u128 ffc_compute_product_approximation(int64_t q, uint64_t w, ffc_value_kind vk) {
  // The required precision is mantissa_explicit_bits + 3 because
  // 1. We need the implicit bit
  // 2. We need an extra bit for rounding purposes
  // 3. We might lose a bit due to the "upperbit" routine (result too small,
  // requiring a shift)
  uint64_t bit_precision = ffc_const(vk, MANTISSA_EXPLICIT_BITS) + 3;
  uint64_t precision_mask = ((uint64_t)(0xFFFFFFFFFFFFFFFF) >> bit_precision);

  // Use FFC_DOUBLE_SMALLEST_POWER_OF_10 (-342) regardless of value kind
  // to index into the shared 128 bit table
  int const index = 2 * (int)(q - FFC_DOUBLE_SMALLEST_POWER_OF_10);

  // For small values of q, e.g., q in [0,27], the answer is always exact
  // because ffc_mul_u64(w, powers_of_five[index]) gives the exact answer.
  ffc_u128 firstproduct = ffc_mul_u64(w, FFC_POWERS_OF_FIVE[index]);
                           
  if ((firstproduct.high & precision_mask) == precision_mask) {
    // could further guard with  (lower + w < lower)
    // regarding the second product, we only need secondproduct.hi, but our
    // expectation is that the compiler will optimize this extra work away if
    // needed.
    ffc_u128 secondproduct = ffc_mul_u64(w, FFC_POWERS_OF_FIVE[index + 1]);

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
ffc_internal ffc_inline int32_t ffc_b10_to_b2(int32_t q) {
  return (((152170 + 65536) * q) >> 16) + 63;
}

// Computes w * 10 ** q.
// The returned value should be a valid number that simply needs to be
// packed. However, in some very rare cases, the computation will fail. In such
// cases, we return an adjusted_mantissa with a negative power of 2: the caller
// should recompute in such cases.
ffc_inline ffc_internal
ffc_adjusted_mantissa ffc_compute_float(int64_t q, uint64_t w, ffc_value_kind vk) {
  ffc_adjusted_mantissa answer;
  if ((w == 0) || (q < ffc_const(vk, SMALLEST_POWER_OF_10))) {
    answer.power2 = 0;
    answer.mantissa = 0;
    // result should be zero
    return answer;
  }
  if (q > ffc_const(vk, LARGEST_POWER_OF_10)) {
    // we want to get infinity:
    answer.power2 = ffc_const(vk, INFINITE_POWER);
    answer.mantissa = 0;
    return answer;
  }
  // At this point in time q is in [powers::smallest_power_of_five,
  // powers::largest_power_of_five].

  // We want the most significant bit of i to be 1. Shift if needed.
  int32_t lz = (int32_t)ffc_count_leading_zeroes(w);
  w <<= lz;

  ffc_u128 product = ffc_compute_product_approximation(q, w, vk);

  // The computed 'product' is always sufficient.
  // Mathematical proof:
  // Noble Mushtak and Daniel Lemire, Fast Number Parsing Without Fallback (to
  // appear) See script/mushtak_lemire.py

  // The "compute_product_approximation" function can be slightly slower than a
  // branchless approach: value128 product = compute_product(q, w); but in
  // practice, we can win big with the compute_product_approximation if its
  // additional branch is easily predicted. Which is data specific.
  int upperbit = (int)(product.high >> 63);
  int shift = upperbit + 64 - ffc_const(vk, MANTISSA_EXPLICIT_BITS) - 3;

  answer.mantissa = product.high >> shift;

  // compute a biased-up power of 2
  answer.power2 = (int32_t)(ffc_b10_to_b2((int32_t)(q)) + upperbit - lz - ffc_const(vk, MINIMUM_EXPONENT));

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
      (answer.mantissa < ((uint64_t)(1) << ffc_const(vk, MANTISSA_EXPLICIT_BITS))) ? 0 : 1;
    return answer;
  } // subnormal

  // usually, we round *up*, but if we fall right in between and we have an
  // even basis, we need to round down
  // We are only concerned with the cases where 5**q fits in single 64-bit word.

  // 'extremely sparse low bits in our product' and
  // 'q is within the round to even range' and
  // 'mantissa lowest 2 bits are exactly 01'
  if ((product.low <= 1) && (q >= ffc_const(vk, MIN_EXPONENT_ROUND_TO_EVEN)) &&
      (q <= ffc_const(vk, MAX_EXPONENT_ROUND_TO_EVEN)) &&
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
  if (answer.mantissa >= ((uint64_t)(2) << ffc_const(vk, MANTISSA_EXPLICIT_BITS))) {
    answer.mantissa = ((uint64_t)(1) << ffc_const(vk, MANTISSA_EXPLICIT_BITS));
    answer.power2++; // undo previous addition
  }

  // normalize to pos INF?
  answer.mantissa &= ~((uint64_t)(1) << ffc_const(vk, MANTISSA_EXPLICIT_BITS));
  if (answer.power2 >= ffc_const(vk, INFINITE_POWER)) { // infinity
    answer.power2 = ffc_const(vk, INFINITE_POWER);
    answer.mantissa = 0;
  }
  return answer;
}

// create an adjusted mantissa, biased by the invalid power2
// for significant digits already multiplied by 10 ** q.
ffc_internal ffc_inline
ffc_adjusted_mantissa ffc_compute_error_scaled(int64_t q, uint64_t w, int lz, ffc_value_kind vk) {
  int hilz = (int)(w >> 63) ^ 1;
  ffc_adjusted_mantissa answer;
  answer.mantissa = w << hilz;
  int bias = ffc_const(vk, MANTISSA_EXPLICIT_BITS) - ffc_const(vk, MINIMUM_EXPONENT);
  answer.power2 = (int32_t)(ffc_b10_to_b2((int32_t)(q)) + bias - hilz - lz - 62 +
                          FFC_INVALID_AM_BIAS);
  return answer;
}

// w * 10 ** q, without rounding the representation up.
// the power2 in the exponent will be adjusted by invalid_am_bias.
ffc_internal ffc_inline
ffc_adjusted_mantissa ffc_compute_error(int64_t q, uint64_t w, ffc_value_kind vk) {
  int32_t lz = (int32_t)ffc_count_leading_zeroes(w);
  w <<= lz;
  ffc_u128 product = ffc_compute_product_approximation(q, w, vk);
  return ffc_compute_error_scaled(q, product.high, lz, vk);
}

/* end section: decimal to binary */

/* section: entrypoint */

ffc_internal ffc_inline
bool ffc_clinger_fast_path_impl(uint64_t mantissa, int64_t exponent, bool is_negative,
                       ffc_value *value, ffc_value_kind value_kind) {
  bool is_double = value_kind == FFC_VALUE_KIND_DOUBLE;
  // The implementation of the Clinger's fast path is convoluted because
  // we want round-to-nearest in all cases, irrespective of the rounding mode
  // selected on the thread.
  // We proceed optimistically, assuming that detail::rounds_to_nearest()
  // returns true.
  if (ffc_const(value_kind, MIN_EXPONENT_FAST_PATH) <= exponent &&
      exponent <= ffc_const(value_kind, MAX_EXPONENT_FAST_PATH)) {
    // Unfortunately, the conventional Clinger's fast path is only possible
    // when the system rounds to the nearest float.
    //
    // We expect the next branch to almost always be selected.
    // We could check it first (before the previous branch), but
    // there might be performance advantages at having the check
    // be last.
    if (ffc_rounds_to_nearest()) {
      // We have that fegetround() == FE_TONEAREST.
      // Next is Clinger's fast path.
      if (mantissa <= ffc_const(value_kind, MAX_MANTISSA_FAST_PATH)) {
        ffc_set_value(value, value_kind, mantissa);

        if (exponent < 0) {
          if (is_double) {
            value->d = value->d / FFC_DOUBLE_POWERS_OF_TEN[-exponent];
          } else {
            value->f = value->f / FFC_FLOAT_POWERS_OF_TEN[-exponent];
          };
        } else {
          if (is_double) {
            value->d = value->d * FFC_DOUBLE_POWERS_OF_TEN[exponent];
          } else {
            value->f = value->f * FFC_FLOAT_POWERS_OF_TEN[exponent];
          };
        }
        if (is_negative) {
          ffc_set_value(value, value_kind, -ffc_read_value(value, value_kind));
        }
        return true;
      }
    } else {
      // We do not have that fegetround() == FE_TONEAREST.
      // Next is a modified Clinger's fast path, inspired by Jakub Jelínek's
      // proposal
      if (exponent >= 0 &&
          mantissa <= ffc_const(value_kind, MAX_MANTISSA)[exponent]) {
#if defined(__clang__) || defined(FFC_32BIT)
        // Clang may map 0 to -0.0 when fegetround() == FE_DOWNWARD
        if (mantissa == 0) {
          ffc_set_value(value, value_kind, is_negative ? -0. : 0.);
          return true;
        }
#endif
        if (is_double) {
          value->d = (double)mantissa * FFC_DOUBLE_POWERS_OF_TEN[exponent];
        } else {
          value->f = (float)mantissa * FFC_FLOAT_POWERS_OF_TEN[exponent];
        }
        if (is_negative) {
          ffc_set_value(value, value_kind, -ffc_read_value(value, value_kind));
        }
        return true;
      }
    }
  }
  return false;
}

ffc_internal ffc_inline
ffc_result ffc_from_chars_advanced(ffc_parsed const pns, ffc_value* value, ffc_value_kind vk) {
  ffc_result answer;

  answer.outcome = FFC_OUTCOME_OK; // be optimistic :')
  answer.ptr = (char*)pns.lastmatch;

  if (!pns.too_many_digits &&
      ffc_clinger_fast_path_impl(pns.mantissa, pns.exponent, pns.negative, value, vk)) {
    ffc_debug("fast path hit");
    return answer;
  }

  ffc_adjusted_mantissa am = ffc_compute_float(pns.exponent, pns.mantissa, vk);
  ffc_debug("am.mantissa: %llu\n", am.mantissa);
  ffc_debug("am.power2:   %d\n", am.power2);
  if (pns.too_many_digits && am.power2 >= 0) {
    ffc_adjusted_mantissa am_plus_one = ffc_compute_float(pns.exponent, pns.mantissa + 1, vk);
    bool equal = am.mantissa == am_plus_one.mantissa && am.power2 == am_plus_one.power2;
    if (!equal) {
      am = ffc_compute_error(pns.exponent, pns.mantissa, vk);
    }
  }
  // If we called ffc_compute_float(pns.exponent, pns.mantissa)
  // and we have an invalid power (am.power2 < 0), then we need to go the long
  // way around again. This is very uncommon.
  if (am.power2 < 0) {
    am = ffc_digit_comp(pns, am, vk);
  }
  ffc_debug("am post mantissa: %llu\n", am.mantissa);
  ffc_debug("am post power2:   %d\n", am.power2);
  ffc_am_to_float(pns.negative, am, value, vk);

  // Test for over/underflow.
  if ((pns.mantissa != 0 && am.mantissa == 0 && am.power2 == 0) ||
      am.power2 == ffc_const(vk, INFINITE_POWER)) {
    answer.outcome = FFC_OUTCOME_OUT_OF_RANGE;
  }
  return answer;
}

ffc_internal ffc_inline
ffc_result ffc_from_chars(char* first, char* last, ffc_parse_options options, ffc_value *value, ffc_value_kind vk) {

  // Alias for parity with cpp code, no feature macros to apply
  ffc_format const fmt = options.format;

  ffc_result answer;
  if ((uint64_t)(fmt & FFC_FORMAT_FLAG_SKIP_WHITE_SPACE)) {
    while ((first != last) && ffc_is_space(*first)) {
      first++;
    }
  }
  if (first == last) {
    answer.outcome = FFC_OUTCOME_INVALID_INPUT;
    answer.ptr = first;
    return answer;
  }
  uint64_t json_mode = (uint64_t)(fmt & FFC_FORMAT_FLAG_BASIC_JSON);
  ffc_parsed pns = ffc_parse_number_string(first, last, options, json_mode);

  #ifdef FFC_DEBUG
  ffc_dump_parsed(pns);
  #endif                          

  if (!pns.valid) {
    if ((uint64_t)(fmt & FFC_FORMAT_FLAG_NO_INFNAN)) {
      answer.outcome = FFC_OUTCOME_INVALID_INPUT;
      answer.ptr = first;
      return answer;
    } else {
      return ffc_parse_infnan(first, last, value, vk, fmt);
    }
  }

  // call overload that takes parsed_number_string directly.
  return ffc_from_chars_advanced(pns, value, vk);
}

ffc_result ffc_from_chars_double_options(const char *start, const char *end, double* out, ffc_parse_options options) {
  // It would be UB to directly use *out as our ffc_value, even though its the same layout
  ffc_value out_value = {0};

  // The all-important call with a constant VALUE_KIND that should cascade in tons of inlining
  ffc_result result = ffc_from_chars((char*)start, (char*)end, options, &out_value, FFC_VALUE_KIND_DOUBLE);
  *out = out_value.d;
  return result;
}
ffc_result ffc_from_chars_double(char const* first, char const* last, double* out) {
  ffc_parse_options options = ffc_parse_options_default();
  return ffc_from_chars_double_options(first, last, out, options);
}
ffc_result ffc_parse_double(size_t len, const char *s, double *out) {
  char *pend = (char*)(s + len);
  return ffc_from_chars_double(s, pend, out);
}
double ffc_parse_double_simple(size_t len, const char *s, ffc_outcome *outcome) {
  double out = 0.0;
  ffc_result result = ffc_parse_double(len, s, &out);
  if (outcome) {
    *outcome = result.outcome;
  }
  return out;
}

ffc_result ffc_from_chars_float_options(const char *start,  const char *end, float* out, ffc_parse_options options) {
  ffc_value out_value = {0};
  ffc_result result = ffc_from_chars((char*)start, (char*)end, options, &out_value, FFC_VALUE_KIND_FLOAT);
  *out = out_value.f;
  return result;
}
ffc_result ffc_from_chars_float(char const* first, char const* last, float* out) {
  ffc_parse_options options = ffc_parse_options_default();
  return ffc_from_chars_float_options(first, last, out, options);
}
ffc_result ffc_parse_float(size_t len, const char *s, float *out) {
  char *pend = (char*)(s + len);
  return ffc_from_chars_float(s, pend, out);
}
float ffc_parse_float_simple(size_t len, const char *s, ffc_outcome *outcome) {
  float out = 0.0;
  ffc_result result = ffc_parse_float(len, s, &out);
  if (outcome) {
    *outcome = result.outcome;
  }
  return out;
}

ffc_result ffc_parse_i64(size_t len, const char input[len], int base, int64_t  *out) {
  char *pend = (char*)(input + len);
  ffc_int_value value_out;
  ffc_result result = ffc_parse_int_string(input, pend, &value_out, FFC_INT_KIND_S64, ffc_parse_options_default(), base);
  *out = value_out.s64;
  return result;
}
ffc_result ffc_parse_u64(size_t len, const char input[len], int base, uint64_t *out) {
  char *pend = (char*)(input + len);
  ffc_int_value value_out;
  ffc_result result = ffc_parse_int_string(input, pend, &value_out, FFC_INT_KIND_U64, ffc_parse_options_default(), base);
  *out = value_out.u64;
  return result;
}
ffc_result ffc_parse_i32(size_t len, const char input[len], int base, int32_t  *out) {
  char *pend = (char*)(input + len);
  ffc_int_value value_out;
  ffc_result result = ffc_parse_int_string(input, pend, &value_out, FFC_INT_KIND_S32, ffc_parse_options_default(), base);
  *out = value_out.s32;
  return result;
}
ffc_result ffc_parse_u32(size_t len, const char input[len], int base, uint32_t *out) {
  char *pend = (char*)(input + len);
  ffc_int_value value_out;
  ffc_result result = ffc_parse_int_string(input, pend, &value_out, FFC_INT_KIND_U32, ffc_parse_options_default(), base);
  *out = value_out.u32;
  return result;
}

#undef FFC_DOUBLE_SMALLEST_POWER_OF_10        
#undef FFC_DOUBLE_LARGEST_POWER_OF_10         
#undef FFC_DOUBLE_SIGN_INDEX                  
#undef FFC_DOUBLE_INFINITE_POWER              
#undef FFC_DOUBLE_MANTISSA_EXPLICIT_BITS      
#undef FFC_DOUBLE_MINIMUM_EXPONENT            
#undef FFC_DOUBLE_MIN_EXPONENT_ROUND_TO_EVEN  
#undef FFC_DOUBLE_MAX_EXPONENT_ROUND_TO_EVEN  
#undef FFC_DOUBLE_MAX_EXPONENT_FAST_PATH      
#undef FFC_DOUBLE_MAX_MANTISSA_FAST_PATH      
#undef FFC_DOUBLE_EXPONENT_MASK               
#undef FFC_DOUBLE_MANTISSA_MASK               
#undef FFC_DOUBLE_HIDDEN_BIT_MASK             
#undef FFC_DOUBLE_MAX_DIGITS                  

#undef FFC_FLOAT_SMALLEST_POWER_OF_10         
#undef FFC_FLOAT_LARGEST_POWER_OF_10          
#undef FFC_FLOAT_SIGN_INDEX                   
#undef FFC_FLOAT_INFINITE_POWER               
#undef FFC_FLOAT_MANTISSA_EXPLICIT_BITS       
#undef FFC_FLOAT_MINIMUM_EXPONENT             
#undef FFC_FLOAT_MIN_EXPONENT_ROUND_TO_EVEN   
#undef FFC_FLOAT_MAX_EXPONENT_ROUND_TO_EVEN   
#undef FFC_FLOAT_MAX_EXPONENT_FAST_PATH       
#undef FFC_FLOAT_MAX_MANTISSA_FAST_PATH       
#undef FFC_FLOAT_EXPONENT_MASK                
#undef FFC_FLOAT_MANTISSA_MASK                
#undef FFC_FLOAT_HIDDEN_BIT_MASK              
#undef FFC_FLOAT_MAX_DIGITS                   

#undef FFC_POWERS_OF_5_NUMBER_OF_ENTRIES

#endif /* FFC_IMPL */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* FFC_H */

