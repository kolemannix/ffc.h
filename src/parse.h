#ifndef FFC_PARSE_H
#define FFC_PARSE_H

#include "common.h"
#include <math.h>

/* section: read digits */

ffc_internal ffc_inline
uint64_t ffc_byteswap(uint64_t val) {
  return (val & 0xFF00000000000000) >> 56 | (val & 0x00FF000000000000) >> 40 |
         (val & 0x0000FF0000000000) >> 24 | (val & 0x000000FF00000000) >> 8  |
         (val & 0x00000000FF000000) <<  8 | (val & 0x0000000000FF0000) << 24 |
         (val & 0x000000000000FF00) << 40 | (val & 0x00000000000000FF) << 56;
}

ffc_internal ffc_inline
uint32_t ffc_byteswap_32(uint32_t val) {
  return (val >> 24) | ((val >> 8) & 0x0000FF00u) | ((val << 8) & 0x00FF0000u) |
         (val << 24);
}

ffc_inline ffc_internal
uint64_t ffc_read8_to_u64(char const *chars) {
  uint64_t val;
  memcpy(&val, chars, sizeof(uint64_t));
#if FFC_IS_BIG_ENDIAN == 1
  // Need to read as-if the number was in little-endian order.
  val = ffc_byteswap(val);
#endif
  return val;
}

// Read 4 UC into a u32. Truncates UC if not char.
ffc_internal ffc_inline uint32_t
ffc_read4_to_u32(char const *chars) {
  uint32_t val;
  memcpy(&val, chars, sizeof(uint32_t));
#if FFC_IS_BIG_ENDIAN == 1
  val = ffc_byteswap_32(val);
#endif
  return val;
}

#ifdef FFC_SSE2

ffc_internal ffc_inline
uint64_t ffc_simd_read8_to_u64_simdreg(__m128i const data) {
  FFC_SIMD_DISABLE_WARNINGS
  __m128i const packed = _mm_packus_epi16(data, data);
#ifdef FFC_64BIT
  return (uint64_t)_mm_cvtsi128_si64(packed);
#else
  uint64_t value;
  // Visual Studio + older versions of GCC don't support _mm_storeu_si64
  _mm_storel_epi64((__m128i*)&value, packed);
  return value;
#endif
  FFC_SIMD_RESTORE_WARNINGS
}

ffc_internal ffc_inline
uint64_t ffc_simd_read8_to_u64(uint16_t const *chars) {
  FFC_SIMD_DISABLE_WARNINGS
  return ffc_simd_read8_to_u64_simdreg(
      _mm_loadu_si128((const __m128i*)chars));
  FFC_SIMD_RESTORE_WARNINGS
}

#elif defined(FFC_NEON)

ffc_internal ffc_inline
uint64_t ffc_simd_read8_to_u64_simdreg(uint16x8_t const data) {
  FFC_SIMD_DISABLE_WARNINGS
  uint8x8_t utf8_packed = vmovn_u16(data);
  return vget_lane_u64(vreinterpret_u64_u8(utf8_packed), 0);
  FFC_SIMD_RESTORE_WARNINGS
}

ffc_internal ffc_inline
uint64_t ffc_simd_read8_to_u64(uint16_t const *chars) {
  FFC_SIMD_DISABLE_WARNINGS
  return ffc_simd_read8_to_u64_simdreg(vld1q_u16(chars));
  FFC_SIMD_RESTORE_WARNINGS
}

#endif // FFC_SSE2

// credit  @aqrit
ffc_internal ffc_inline uint32_t
ffc_parse_eight_digits_unrolled_swar(uint64_t val) {
  uint64_t const mask = 0x000000FF000000FF;
  uint64_t const mul1 = 0x000F424000000064; // 100 + (1000000ULL << 32)
  uint64_t const mul2 = 0x0000271000000001; // 1 + (10000ULL << 32)
  val -= 0x3030303030303030;
  val = (val * 10) + (val >> 8); // val = (val * 2561) >> 8;
  val = (((val & mask) * mul1) + (((val >> 16) & mask) * mul2)) >> 32;
  return (uint32_t)val;
}

// Call this if chars are definitely 8 digits.
ffc_internal ffc_inline
uint32_t ffc_parse_eight_digits_unrolled(char const *chars) {
  return ffc_parse_eight_digits_unrolled_swar(ffc_read8_to_u64(chars)); // truncation okay
}

// credit @aqrit
ffc_internal ffc_inline bool
ffc_is_made_of_eight_digits_fast(uint64_t val) {
  return !((((val + 0x4646464646464646) | (val - 0x3030303030303030)) &
            0x8080808080808080));
}

ffc_internal ffc_inline bool
ffc_is_made_of_four_digits_fast(uint32_t val) {
  return !((((val + 0x46464646) | (val - 0x30303030)) & 0x80808080));
}

ffc_internal ffc_inline
uint32_t ffc_parse_four_digits_unrolled(uint32_t val) {
  val -= 0x30303030;
  val = (val * 10) + (val >> 8);
  return (((val & 0x00FF00FF) * 0x00640001) >> 16) & 0xFFFF;
}

#ifdef FFC_HAS_SIMD

// Call this if chars might not be 8 digits.
// Using this style (instead of is_made_of_eight_digits_fast() then
// parse_eight_digits_unrolled()) ensures we don't load SIMD registers twice.
ffc_internal ffc_inline
bool ffc_simd_parse_if_eight_digits_unrolled_simd(uint16_t const *chars, uint64_t* i) {
#ifdef FFC_SSE2
  FFC_SIMD_DISABLE_WARNINGS
  __m128i const data =
      _mm_loadu_si128((__m128i const *)chars);

  // (x - '0') <= 9
  // http://0x80.pl/articles/simd-parsing-int-sequences.html
  __m128i const t0 = _mm_add_epi16(data, _mm_set1_epi16(32720));
  __m128i const t1 = _mm_cmpgt_epi16(t0, _mm_set1_epi16(-32759));

  if (_mm_movemask_epi8(t1) == 0) {
    *i = *i * 100000000 + ffc_parse_eight_digits_unrolled_swar(ffc_simd_read8_to_u64_simdreg(data));
    return true;
  } else
    return false;
  FFC_SIMD_RESTORE_WARNINGS
#elif defined(FFC_NEON)
  FFC_SIMD_DISABLE_WARNINGS
  uint16x8_t const data = vld1q_u16(chars);

  // (x - '0') <= 9
  // http://0x80.pl/articles/simd-parsing-int-sequences.html
  uint16x8_t const t0 = vsubq_u16(data, vmovq_n_u16('0'));
  uint16x8_t const mask = vcltq_u16(t0, vmovq_n_u16('9' - '0' + 1));

  if (vminvq_u16(mask) == 0xFFFF) {
    *i = *i * 100000000 + ffc_parse_eight_digits_unrolled_swar(ffc_simd_read8_to_u64_simdreg(data));
    return true;
  } else
    return false;
  FFC_SIMD_RESTORE_WARNINGS
#else
  (void)chars;
  (void)i;
  return false;
#endif // FFC_SSE2
}

#endif // FFC_HAS_SIMD

ffc_internal ffc_inline void
ffc_loop_parse_if_eight_digits(char const **p, char const *const pend,
                           uint64_t* i) {
  // optimizes better than parse_if_eight_digits_unrolled() for char.
  while ((pend - *p >= 8) &&
         ffc_is_made_of_eight_digits_fast(ffc_read8_to_u64(*p))) {
    *i = (*i * 100000000) +
        ffc_parse_eight_digits_unrolled_swar(ffc_read8_to_u64(*p)); 
        // in rare cases, this will overflow, but that's ok
    *p += 8;
  }
}

/* section end: read digits */

/* section: parse */

ffc_parse_options ffc_parse_options_default(void) {
  ffc_parse_options options;
  options.format = FFC_PRESET_GENERAL;
  options.decimal_point = '.';
  return options;
}

typedef struct ffc_parsed {
  int64_t  exponent;
  uint64_t mantissa;
  /* Populated on error; indicates where parsing failed */
  char     const *lastmatch;
  bool     negative;
  bool     valid;
  bool     too_many_digits;
  char*    int_part_start;
  size_t   int_part_len;
  char*    fraction_part_start;
  size_t   fraction_part_len;

  ffc_parse_outcome outcome;
} ffc_parsed;

ffc_internal ffc_inline
ffc_parsed ffc_report_parse_error(char const *p, ffc_parse_outcome outcome) {
  ffc_parsed answer;
  answer.valid = false;
  answer.lastmatch = p;
  answer.outcome = outcome;
  return answer;
}

ffc_internal ffc_inline
ffc_parsed ffc_parse_number_string(
    char const *p,
    // CONTRACT: p < pend
    char const *pend,
    ffc_parse_options const options,
    // explicitly passed to encourage optimizer to specialize
    bool const basic_json_fmt
) {
  ffc_format fmt = options.format;
  char decimal_point = options.decimal_point;

  FFC_DEBUG_ASSERT(fmt != 0);

  ffc_parsed answer = {0};
  answer.negative = (*p == '-');
  // C++17 20.19.3.(7.1) explicitly forbids '+' sign here
  // so we only allow it if we've been told to, and are not in json mode
  bool allow_leading_plus = fmt & FFC_FORMAT_FLAG_ALLOW_LEADING_PLUS;
  if ((*p == '-') || (uint64_t)(allow_leading_plus && !basic_json_fmt && *p == '+')) {
    ++p;
    if (p == pend) {
      return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_MISSING_INTEGER_OR_DOT_AFTER_SIGN);
    }
    if (basic_json_fmt) {
      if (!ffc_is_integer(*p)) { // a sign must be followed by an integer
        return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_JSON_MISSING_INTEGER_AFTER_SIGN);
      }
    } else {
      // a sign must be followed by an integer or the dot
      if (!ffc_is_integer(*p) && (*p != decimal_point)) { 
        return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_MISSING_INTEGER_OR_DOT_AFTER_SIGN);
      }
    }
  }

  // phew, we've found the digits
  char const *const start_digits = p;

  uint64_t i = 0; // an unsigned int avoids signed overflows (which are bad)

  while ((p != pend) && ffc_is_integer(*p)) {
    // Horner's method: only ever multiplies by the constant 10
    // avoiding variable power-of-10 multiplies

    // might overflow, we will handle the overflow later
    uint64_t digit_value = (uint64_t)(*p - '0');
    i = (10 * i) + digit_value; 
    ++p;
  }

  char const *const end_of_integer_part = p;

  int64_t digit_count = (int64_t)(end_of_integer_part - start_digits);
  answer.int_part_start = (char*)start_digits;
  answer.int_part_len = (size_t)(digit_count);

  if (basic_json_fmt) {
    // at least 1 digit in integer part
    if (digit_count == 0) {
      return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_JSON_NO_DIGITS_IN_INTEGER_PART);
    }
    // no leading zeros
    if ((start_digits[0] == '0' && digit_count > 1)) {
      return ffc_report_parse_error(start_digits, FFC_PARSE_OUTCOME_JSON_LEADING_ZEROS_IN_INTEGER_PART);
    }
  }

  int64_t exponent = 0;
  bool const has_decimal_point = (p != pend) && (*p == decimal_point);

  /* post-decimal exponential part (calculates a negative exponent) */
  if (has_decimal_point) {
    ++p;
    char const *before = p; 
    // can occur at most twice without overflowing, but let it occur more, since
    // for integers with many digits, digit parsing is the primary bottleneck.
    ffc_loop_parse_if_eight_digits(&p, pend, &i);

    while ((p != pend) && ffc_is_integer(*p)) {
      uint8_t digit = (uint8_t)(*p - (char)('0'));
      ++p;
      i = i * 10 + digit; // in rare cases, this will overflow, but that's ok
    }

    // pre: i = 123, digit_count = 3
    // 123.456
    //        ^ pend
    //        ^ p
    //     ^ before
    // exponent = -3
    // i = 123456
    // digit_count = 3 - (-3) = 6
    exponent = before - p;
    answer.fraction_part_start = (char*)before;
    answer.fraction_part_len = (size_t)(p - before);
    digit_count -= exponent;
  }
  if (basic_json_fmt) {
    // at least 1 digit in fractional part
    if (has_decimal_point && exponent == 0) {
      return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_JSON_NO_DIGITS_IN_FRACTIONAL_PART);
    }
  } else if (digit_count == 0) { // we must have encountered at least one integer!
    return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_NO_DIGITS_IN_MANTISSA);
  }

  /* explicit exponential part */
  int64_t exp_number = 0; 
  if (((uint64_t)(fmt & FFC_FORMAT_FLAG_SCIENTIFIC) && (p != pend) &&
       (('e' == *p) || ('E' == *p))) ||
      ((uint64_t)(fmt & FFC_FORMAT_FLAG_BASIC_FORTRAN) && (p != pend) &&
       (('+' == *p) || ('-' == *p) || ('d' == *p) ||
        ('D' == *p)))) {
    char const *location_of_e = p;
    if (('e' == *p) || ('E' == *p) || ('d' == *p) ||
        ('D' == *p)) {
      ++p;
    }
    bool neg_exp = false;
    if ((p != pend) && ('-' == *p)) {
      neg_exp = true;
      ++p;
    } else if ((p != pend) && ('+' == *p)) { // '+' on exponent is allowed by C++17 20.19.3.(7.1)
      ++p;
    }
    if ((p == pend) || !ffc_is_integer(*p)) {
      if (!(uint64_t)(fmt & FFC_FORMAT_FLAG_FIXED)) {
        // The exponential part is invalid for scientific notation, so it must
        // be a trailing token for fixed notation. However, fixed notation is
        // disabled, so report a scientific notation error.
        return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_MISSING_EXPONENTIAL_PART);
      }
      // Otherwise, we will be ignoring the 'e'.
      p = location_of_e;
    } else {
      while ((p != pend) && ffc_is_integer(*p)) {
        uint8_t digit = (uint8_t)(*p - '0');
        if (exp_number < 0x10000000) {
          exp_number = 10 * exp_number + digit;
        }
        ++p;
      }
      if (neg_exp) {
        exp_number = -exp_number;
      }
      exponent += exp_number;
    }
  } else {
    // If it scientific and not fixed, we have to bail out.
    if ((uint64_t)(fmt & FFC_FORMAT_FLAG_SCIENTIFIC) &&
        !(uint64_t)(fmt & FFC_FORMAT_FLAG_FIXED)) {
      return ffc_report_parse_error(p, FFC_PARSE_OUTCOME_MISSING_EXPONENTIAL_PART);
    }
  }
  answer.lastmatch = p;
  answer.valid = true;

  // If we frequently had to deal with long strings of digits,
  // we could extend our code by using a 128-bit integer instead
  // of a 64-bit integer. However, this is uncommon.
  //
  // We can deal with up to 19 digits.
  if (digit_count > 19) { // this is uncommon
    ffc_debug("high digit_count %lld\n", digit_count);
    // It is possible that the integer had an overflow.
    // We have to handle the case where we have 0.0000somenumber.
    // We need to be mindful of the case where we only have zeroes...
    // E.g., 0.000000000...000.
    char const *start = start_digits;
    while ((start != pend) && (*start == '0' || *start == decimal_point)) {
      if (*start == '0') {
        digit_count--;
      }
      start++;
    }

    if (digit_count > 19) {
      answer.too_many_digits = true;
      // Let us start again, this time, avoiding overflows.
      // We don't need to call if is_integer, since we use the
      // pre-tokenized spans from above.
      i = 0;
      p = answer.int_part_start;
      char const *int_end = p + answer.int_part_len;
      uint64_t const minimal_nineteen_digit_integer = 1000000000000000000;
      while ((i < minimal_nineteen_digit_integer) && (p != int_end)) {
        i = i * 10 + (uint64_t)(*p - '0');
        ++p;
      }
      if (i >= minimal_nineteen_digit_integer) { // We have a big integer
        exponent = end_of_integer_part - p + exp_number;
      } else { // We have a value with a fractional component.
        p = answer.fraction_part_start;
        char const *frac_end = p + answer.fraction_part_len;
        while ((i < minimal_nineteen_digit_integer) && (p != frac_end)) {
          i = i * 10 + (uint64_t)(*p - '0');
          ++p;
        }
        exponent = answer.fraction_part_start - p + exp_number;
      }
      // We have now corrected both exponent and i, to a truncated value
    }
  }
  answer.exponent = exponent;
  answer.mantissa = i;
  return answer;
}

/**
 * Special case +inf, -inf, nan, infinity, -infinity.
 * The case comparisons could be made much faster given that we know that the
 * strings a null-free and fixed.
 **/
ffc_internal ffc_inline
ffc_result ffc_parse_infnan(
    char *first, char *last,
    ffc_value *value,
    ffc_value_kind vk,
    ffc_format fmt
) {
  ffc_debug("parse_infnan\n");
  ffc_result answer;
  answer.ptr = first;
  answer.outcome = FFC_OUTCOME_OK; // be optimistic
  // assume first < last, so dereference without checks;
  bool const minus_sign = (*first == '-');
  // C++17 20.19.3.(7.1) explicitly forbids '+' sign here
  if ((*first == '-') ||
      ((uint64_t)(fmt & FFC_FORMAT_FLAG_ALLOW_LEADING_PLUS) &&
       (*first == '+'))) {
    ++first;
  }
  if (last - first >= 3) {
    if (ffc_strncasecmp3(first, "nan", 1)) {
      answer.ptr = (first += 3);

      // The macro casts the literal AFTER applying the negative sign. This is ok:
      // -NAN is a double negative NaN, and (float)(-NAN) correctly produces a negative float NaN.
      // Same for infinity — (float)(-INFINITY) gives negative float infinity
      ffc_set_value(value, vk, minus_sign ? -NAN : NAN);
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
    if (ffc_strncasecmp3(first, "infinity", 1)) {
      if ((last - first >= 8) &&
          ffc_strncasecmp5(first + 3, (char*)&"infinity"[3], 1)) {
        answer.ptr = first + 8;
      } else {
        answer.ptr = first + 3;
      }
      ffc_set_value(value, vk, minus_sign ? -INFINITY : INFINITY);
      return answer;
    }
  }
  answer.outcome = FFC_OUTCOME_INVALID_INPUT;
  return answer;
}

ffc_internal ffc_inline
ffc_result ffc_parse_int_string(
    char const *p,
    char const *pend,
    ffc_int_value *value,
    ffc_int_kind ik,
    ffc_parse_options options,
    int const base
  ) {
  ffc_debug("input '%.*s'... ", (int)(pend - p), p);
  ffc_format const fmt = options.format;

  if ((uint64_t)(fmt & FFC_FORMAT_FLAG_SKIP_WHITE_SPACE)) {
    while ((p != pend) && ffc_is_space(*p)) {
      p++;
    }
  }

  if (p == pend || base < 2 || base > 36) {
    return (ffc_result){ .ptr = (char*)p, .outcome = FFC_OUTCOME_INVALID_INPUT };
  }

  ffc_result answer;
  char const *const first = p;

  bool const negative = (*p == (char)('-'));

  if (!ffc_int_kind_is_signed(ik) && negative) {
    answer.outcome = FFC_OUTCOME_INVALID_INPUT;
    answer.ptr = (char*)first;
    return answer;
  }
  if ((*p == (char)('-')) ||
      ((uint64_t)(fmt & FFC_FORMAT_FLAG_ALLOW_LEADING_PLUS) && (*p == (char)('+')))) {
    ++p;
  }

  char const *const start_num = p;

  while (p != pend && *p == (char)('0')) {
    ++p;
  }

  bool const has_leading_zeros = p > start_num;

  char const *const start_digits = p;

  uint64_t i = 0;
  if (base == 10) {
    ffc_loop_parse_if_eight_digits(&p, pend, &i); // use SIMD if possible
  }
  while (p != pend) {
    uint8_t digit = ffc_char_to_digit(*p);
    if (digit >= base) {
      break;
    }
    i = (uint64_t)(base) * i + digit; // might overflow, check this later
    p++;
  }

  size_t digit_count = (size_t)(p - start_digits);

  if (digit_count == 0) {
    if (has_leading_zeros) {
      *value = (ffc_int_value){0}; // Largest variants are defined first so this will clear the entire union
      answer.outcome = FFC_OUTCOME_OK;
      answer.ptr = p;
    } else {
      answer.outcome = FFC_OUTCOME_INVALID_INPUT;
      answer.ptr = first;
    }
    return answer;
  }

  answer.ptr = p;

  // check u64 overflow
  size_t max_digits = ffc_max_digits_u64(base);
  ffc_debug("digit_count %d, max_digits: %d\n", digit_count, max_digits);
  if (digit_count > max_digits) {
    answer.outcome = FFC_OUTCOME_OUT_OF_RANGE;
    return answer;
  }
  // this check can be eliminated for all other types, but they will all require
  // a max_digits(base) equivalent
  if (digit_count == max_digits && i < ffc_min_safe_u64_of_base(base)) {
    answer.outcome = FFC_OUTCOME_OUT_OF_RANGE;
    return answer;
  }

  ffc_debug("i is %lld, ik is %s\n", i, (ik == FFC_INT_KIND_U64 ? "u64" :
                                         ik == FFC_INT_KIND_S64 ? "s64" :
                                         ik == FFC_INT_KIND_U32 ? "u32" :
                                         ik == FFC_INT_KIND_S32 ? "s32" : "?"));
  // check other types overflow
  if (ik != FFC_INT_KIND_U64) {
    // Allow 1 greater magnitude when negative
    if (i > ffc_int_value_max(ik) + (uint64_t)(negative)) {
      answer.outcome = FFC_OUTCOME_OUT_OF_RANGE;
      return answer;
    }
  }

  // All signed conversion goes through unsigned arithmetic to avoid UB
  if (negative) {
    uint64_t neg_i = ~i + 1; // This is the two's complement negation
    // write into the signed slot via union
    switch (ik) {
      case FFC_INT_KIND_S64: value->s64 = (int64_t)neg_i; break; // implementation-defined, but works everywhere
      case FFC_INT_KIND_S32: value->s32 = (int32_t)(int64_t)neg_i; break;
      // unsigned kinds can't be negative — guarded by the range check above
      // case FFC_INT_KIND_S16: value.i16 = (int16_t)(int64_t)neg_i; break;
      // case FFC_INT_KIND_S8:  value.i8  = (int8_t)(int64_t)neg_i;  break;
    }
  } else {
    switch (ik) {
      case FFC_INT_KIND_S64: value->s64 = (int64_t)i;          break; 
      case FFC_INT_KIND_S32: value->s32 = (int32_t)(int64_t)i; break;
      case FFC_INT_KIND_U64: value->u64 = i;                   break;
      case FFC_INT_KIND_U32: value->u32 = (uint32_t)i;         break;
    }
  }

  answer.outcome = FFC_OUTCOME_OK;
  return answer;
}

#ifdef FFC_DEBUG 

#include <stdio.h>
ffc_internal ffc_inline
void ffc_dump_parsed(ffc_parsed const p) {
  (void)p;
  ffc_debug("mantissa: %llu\n", (unsigned long long)p.mantissa);
  ffc_debug("exponent: %lld\n", (long long)p.exponent);
  ffc_debug("negative: %d\n", p.negative);
  ffc_debug("valid: %d\n", p.valid);
  ffc_debug("too_many_digits: %d\n", p.too_many_digits);
  ffc_debug("int_part_len: %zu\n", p.int_part_len);
  ffc_debug("fraction_part_len: %zu\n", p.fraction_part_len);
}

#endif

/* section end: parse */

#endif // FFC_PARSE_H
