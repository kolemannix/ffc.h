#ifndef JKN_FF_PARSE_H
#define JKN_FF_PARSE_H

#include "common.h"

/* section: read digits */

jkn_ff_inline uint64_t byteswap(uint64_t val) {
  return (val & 0xFF00000000000000) >> 56 | (val & 0x00FF000000000000) >> 40 |
         (val & 0x0000FF0000000000) >> 24 | (val & 0x000000FF00000000) >> 8  |
         (val & 0x00000000FF000000) <<  8 | (val & 0x0000000000FF0000) << 24 |
         (val & 0x000000000000FF00) << 40 | (val & 0x00000000000000FF) << 56;
}

jkn_ff_inline uint32_t byteswap_32(uint32_t val) {
  return (val >> 24) | ((val >> 8) & 0x0000FF00u) | ((val << 8) & 0x00FF0000u) |
         (val << 24);
}

jkn_ff_inline uint64_t
jkn_ff_read8_to_u64(char const *chars) {
  uint64_t val;
  memcpy(&val, chars, sizeof(uint64_t));
#if FASTFLOAT_IS_BIG_ENDIAN == 1
  // Need to read as-if the number was in little-endian order.
  val = byteswap(val);
#endif
  return val;
}

// Read 4 UC into a u32. Truncates UC if not char.
jkn_ff_internal jkn_ff_inline uint32_t
jkn_ff_read4_to_u32(char const *chars) {
  uint32_t val;
  memcpy(&val, chars, sizeof(uint32_t));
#if FASTFLOAT_IS_BIG_ENDIAN == 1
  val = byteswap_32(val);
#endif
  return val;
}

#ifdef FASTFLOAT_SSE2

jkn_ff_inline uint64_t simd_read8_to_u64_simdreg(__m128i const data) {
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  __m128i const packed = _mm_packus_epi16(data, data);
#ifdef FASTFLOAT_64BIT
  return (uint64_t)_mm_cvtsi128_si64(packed);
#else
  uint64_t value;
  // Visual Studio + older versions of GCC don't support _mm_storeu_si64
  _mm_storel_epi64(&value, packed);
  return value;
#endif
  FASTFLOAT_SIMD_RESTORE_WARNINGS
}

jkn_ff_inline uint64_t simd_read8_to_u64(char16_t const *chars) {
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  return simd_read8_to_u64(
      _mm_loadu_si128(reinterpret_cast<__m128i const *>(chars)));
  FASTFLOAT_SIMD_RESTORE_WARNINGS
}

#elif defined(FASTFLOAT_NEON)

jkn_ff_inline uint64_t jkn_ff_simd_read8_to_u64_simdreg(uint16x8_t const data) {
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  uint8x8_t utf8_packed = vmovn_u16(data);
  return vget_lane_u64(vreinterpret_u64_u8(utf8_packed), 0);
  FASTFLOAT_SIMD_RESTORE_WARNINGS
}

jkn_ff_inline uint64_t jkn_ff_simd_read8_to_u64(uint16_t const *chars) {
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  return jkn_ff_simd_read8_to_u64_simdreg(vld1q_u16(chars));
  FASTFLOAT_SIMD_RESTORE_WARNINGS
}

#endif // FASTFLOAT_SSE2

// dummy for compile nocommit
//uint64_t jkn_ff_simd_read8_to_u64(char const *) {
//  return 0;
//}

// credit  @aqrit
jkn_ff_internal jkn_ff_inline uint32_t
jkn_ff_parse_eight_digits_unrolled_swar(uint64_t val) {
  uint64_t const mask = 0x000000FF000000FF;
  uint64_t const mul1 = 0x000F424000000064; // 100 + (1000000ULL << 32)
  uint64_t const mul2 = 0x0000271000000001; // 1 + (10000ULL << 32)
  val -= 0x3030303030303030;
  val = (val * 10) + (val >> 8); // val = (val * 2561) >> 8;
  val = (((val & mask) * mul1) + (((val >> 16) & mask) * mul2)) >> 32;
  return (uint32_t)val;
}

// Call this if chars are definitely 8 digits.
jkn_ff_internal jkn_ff_inline
uint32_t jkn_ff_parse_eight_digits_unrolled(char const *chars) {
  if (!FASTFLOAT_HAS_SIMD) {
    return jkn_ff_parse_eight_digits_unrolled_swar(jkn_ff_read8_to_u64(chars)); // truncation okay
  }
  return jkn_ff_parse_eight_digits_unrolled_swar(jkn_ff_simd_read8_to_u64((uint16_t*)chars));
}

// credit @aqrit
jkn_ff_internal jkn_ff_inline bool
is_made_of_eight_digits_fast(uint64_t val) {
  return !((((val + 0x4646464646464646) | (val - 0x3030303030303030)) &
            0x8080808080808080));
}

jkn_ff_internal jkn_ff_inline bool
is_made_of_four_digits_fast(uint32_t val) {
  return !((((val + 0x46464646) | (val - 0x30303030)) & 0x80808080));
}

jkn_ff_internal jkn_ff_inline
uint32_t jkn_ff_parse_four_digits_unrolled(uint32_t val) {
  val -= 0x30303030;
  val = (val * 10) + (val >> 8);
  return (((val & 0x00FF00FF) * 0x00640001) >> 16) & 0xFFFF;
}

#ifdef FASTFLOAT_HAS_SIMD

// Call this if chars might not be 8 digits.
// Using this style (instead of is_made_of_eight_digits_fast() then
// parse_eight_digits_unrolled()) ensures we don't load SIMD registers twice.
jkn_ff_internal jkn_ff_inline
bool jkn_ff_simd_parse_if_eight_digits_unrolled_simd(uint16_t const *chars, uint64_t* i) {
#ifdef FASTFLOAT_SSE2
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  __m128i const data =
      _mm_loadu_si128((__m128i const *)chars);

  // (x - '0') <= 9
  // http://0x80.pl/articles/simd-parsing-int-sequences.html
  __m128i const t0 = _mm_add_epi16(data, _mm_set1_epi16(32720));
  __m128i const t1 = _mm_cmpgt_epi16(t0, _mm_set1_epi16(-32759));

  if (_mm_movemask_epi8(t1) == 0) {
    *i = *i * 100000000 + jkn_ff_parse_eight_digits_unrolled_swar(jkn_ff_simd_read8_to_u64_simdreg(data));
    return true;
  } else
    return false;
  FASTFLOAT_SIMD_RESTORE_WARNINGS
#elif defined(FASTFLOAT_NEON)
  FASTFLOAT_SIMD_DISABLE_WARNINGS
  uint16x8_t const data = vld1q_u16(chars);

  // (x - '0') <= 9
  // http://0x80.pl/articles/simd-parsing-int-sequences.html
  uint16x8_t const t0 = vsubq_u16(data, vmovq_n_u16('0'));
  uint16x8_t const mask = vcltq_u16(t0, vmovq_n_u16('9' - '0' + 1));

  if (vminvq_u16(mask) == 0xFFFF) {
    *i = *i * 100000000 + jkn_ff_parse_eight_digits_unrolled_swar(jkn_ff_simd_read8_to_u64_simdreg(data));
    return true;
  } else
    return false;
  FASTFLOAT_SIMD_RESTORE_WARNINGS
#else
  (void)chars;
  (void)i;
  return false;
#endif // FASTFLOAT_SSE2
}

#endif // FASTFLOAT_HAS_SIMD

// dummy for compile nocommit
//bool simd_parse_if_eight_digits_unrolled(char const *, uint64_t &) {
//  return 0;
//}

//non-char; forget about it
//template <typename UC, FASTFLOAT_ENABLE_IF(!std::is_same<UC, char>::value) = 0>
//jkn_ff_inline FASTFLOAT_CONSTEXPR20 void
//loop_parse_if_eight_digits(UC const *&p, UC const *const pend, uint64_t &i) {
//  if (!has_simd_opt<UC>()) {
//    return;
//  }
//  while ((std::distance(p, pend) >= 8) &&
//         simd_parse_if_eight_digits_unrolled(
//             p, i)) { // in rare cases, this will overflow, but that's ok
//    p += 8;
//  }
//}

jkn_ff_internal jkn_ff_inline void
jkn_ff_loop_parse_if_eight_digits(char const **p, char const *const pend,
                           uint64_t* i) {
  // optimizes better than parse_if_eight_digits_unrolled() for char.
  while ((pend - *p >= 8) &&
         is_made_of_eight_digits_fast(jkn_ff_read8_to_u64(*p))) {
    *i = (*i * 100000000) +
        jkn_ff_parse_eight_digits_unrolled_swar(jkn_ff_read8_to_u64(*p)); 
        // in rare cases, this will overflow, but that's ok
    *p += 8;
  }
}

/* section end: read digits */

/* section: parse */

typedef uint64_t jkn_ff_chars_format;
enum jkn_ff_chars_format_bits {
  JKN_FF_FORMAT_FLAG_SCIENTIFIC         = 1ULL << 0,
  JKN_FF_FORMAT_FLAG_FIXED              = 1ULL << 2, // Fixed the gap here
  JKN_FF_FORMAT_FLAG_HEX                = 1ULL << 3,
  JKN_FF_FORMAT_FLAG_NO_INFNAN          = 1ULL << 4,
  JKN_FF_FORMAT_FLAG_BASIC_JSON         = 1ULL << 5,
  JKN_FF_FORMAT_FLAG_BASIC_FORTRAN      = 1ULL << 6,
  JKN_FF_FORMAT_FLAG_ALLOW_LEADING_PLUS = 1ULL << 7,
  JKN_FF_FORMAT_FLAG_SKIP_WHITE_SPACE   = 1ULL << 8,

  /* Presets */
  JKN_FF_PRESET_GENERAL = JKN_FF_FORMAT_FLAG_FIXED | JKN_FF_FORMAT_FLAG_SCIENTIFIC,
  
  JKN_FF_PRESET_JSON = JKN_FF_FORMAT_FLAG_BASIC_JSON | 
                       JKN_FF_PRESET_GENERAL | 
                       JKN_FF_FORMAT_FLAG_NO_INFNAN,

  JKN_FF_PRESET_JSON_OR_INFNAN = JKN_FF_FORMAT_FLAG_BASIC_JSON | 
                                 JKN_FF_PRESET_GENERAL,

  JKN_FF_PRESET_FORTRAN = JKN_FF_FORMAT_FLAG_BASIC_FORTRAN | 
                          JKN_FF_PRESET_GENERAL,

  /* Force the enum to a 64-bit representation in debuggers */
  _JKN_FF_FORMAT_FORCE_64 = 0xFFFFFFFFFFFFFFFFULL
};

typedef struct jkn_ff_parse_options {
  /** Which number formats are accepted */
  jkn_ff_chars_format format;
  /** The character used as decimal point */
  // nocommit: make zero-init use '.' somehow - just communicate that '\0' won't fly?
  char decimal_point;
  /** The base used only for integers */
  // nocommit: make zero-init use '10' somehow - just communicate that 0 will use 10
  int base;
} jkn_ff_parse_options;

typedef enum jkn_ff_parse_outcome {
  no_error = 0,
  // [JSON-only] The minus sign must be followed by an integer.
  json_missing_integer_after_sign = 1,
  // A sign must be followed by an integer or dot.
  missing_integer_or_dot_after_sign = 2,
  // [JSON-only] The integer part must not have leading zeros.
  json_leading_zeros_in_integer_part = 3,
  // [JSON-only] The integer part must have at least one digit.
  json_no_digits_in_integer_part = 4,
  // [JSON-only] If there is a decimal point, there must be digits in the
  // fractional part.
  json_no_digits_in_fractional_part = 5,
  // The mantissa must have at least one digit.
  no_digits_in_mantissa = 6,
  // Scientific notation requires an exponential part.
  missing_exponential_part = 7,
} jkn_ff_parse_outcome;

typedef struct jkn_ff_parsed {
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

  jkn_ff_parse_outcome outcome;
} jkn_ff_parsed;

jkn_ff_internal jkn_ff_inline
jkn_ff_parsed jkn_ff_report_parse_error(char const *p, jkn_ff_parse_outcome outcome) {
  jkn_ff_parsed answer;
  answer.valid = false;
  answer.lastmatch = p;
  answer.outcome = outcome;
  return answer;
}

jkn_ff_internal jkn_ff_inline
jkn_ff_parsed jkn_ff_parse_number_string(
    char const *p,
    // CONTRACT: p < pend
    char const *pend,
    jkn_ff_parse_options const options,
    // explicitly passed to encourage optimizer to specialize
    bool const basic_json_fmt
) {
  jkn_ff_chars_format fmt = options.format;

  // nocommit silly branch
  if (fmt == 0) {
    fmt = JKN_FF_PRESET_GENERAL;
  }
  // nocommit kinda silly branch
  char const decimal_point = options.decimal_point == '\0' ? '.' : options.decimal_point;

  jkn_ff_parsed answer = {0};
  answer.negative = (*p == '-');
  // C++17 20.19.3.(7.1) explicitly forbids '+' sign here
  // so we only allow it if we've been told to be in json mode
  // nocommit: what does IEEE say? does it talk about the text format?
  if ((*p == '-') || (uint64_t)((fmt & JKN_FF_FORMAT_FLAG_ALLOW_LEADING_PLUS) && !basic_json_fmt && *p == '+')) {
    ++p;
    if (p == pend) {
      return jkn_ff_report_parse_error(p, missing_integer_or_dot_after_sign);
    }
    if (basic_json_fmt) {
      if (!jkn_ff_is_integer(*p)) { // a sign must be followed by an integer
        return jkn_ff_report_parse_error(p, json_missing_integer_after_sign);
      }
    } else {
      // a sign must be followed by an integer or the dot
      if (!jkn_ff_is_integer(*p) && (*p != decimal_point)) { 
        return jkn_ff_report_parse_error(p, missing_integer_or_dot_after_sign);
      }
    }
  }

  // phew, we've found the digits
  char const *const start_digits = p;

  uint64_t i = 0; // an unsigned int avoids signed overflows (which are bad)

  // nocommit why scan instead of using that SWAR detect digits helper
  while ((p != pend) && jkn_ff_is_integer(*p)) {
    // Horner's method: only ever multiplies by the constant 10
    // avoiding variable power-of-10 multiplies

    // might overflow, we will handle the overflow later
    uint64_t digit_value = (uint64_t)(*p - '0');
    i = (10 * i) + digit_value; 
    ++p;
  }

  char const *const end_of_integer_part = p;

  int64_t digit_count = (int64_t)(end_of_integer_part - start_digits);
  // nocommit how do I drop the const without an arbitrary possible erroneous cast
  answer.int_part_start = (char*)start_digits;
  answer.int_part_len = (size_t)(digit_count);

  if (basic_json_fmt) {
    // at least 1 digit in integer part
    if (digit_count == 0) {
      return jkn_ff_report_parse_error(p, json_no_digits_in_integer_part);
    }
    // no leading zeros
    if ((start_digits[0] == '0' && digit_count > 1)) {
      return jkn_ff_report_parse_error(start_digits, json_leading_zeros_in_integer_part);
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
    jkn_ff_loop_parse_if_eight_digits(&p, pend, &i);

    while ((p != pend) && jkn_ff_is_integer(*p)) {
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
      return jkn_ff_report_parse_error(p, json_no_digits_in_fractional_part);
    }
  } else if (digit_count == 0) { // we must have encountered at least one integer!
    return jkn_ff_report_parse_error(p, no_digits_in_mantissa);
  }

  /* explicit exponential part */
  int64_t exp_number = 0; 
  if (((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_SCIENTIFIC) && (p != pend) &&
       (('e' == *p) || ('E' == *p))) ||
      ((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_BASIC_FORTRAN) && (p != pend) &&
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
    if ((p == pend) || !jkn_ff_is_integer(*p)) {
      if (!(uint64_t)(fmt & JKN_FF_FORMAT_FLAG_FIXED)) {
        // The exponential part is invalid for scientific notation, so it must
        // be a trailing token for fixed notation. However, fixed notation is
        // disabled, so report a scientific notation error.
        return jkn_ff_report_parse_error(p, missing_exponential_part);
      }
      // Otherwise, we will be ignoring the 'e'.
      p = location_of_e;
    } else {
      while ((p != pend) && jkn_ff_is_integer(*p)) {
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
    if ((uint64_t)(fmt & JKN_FF_FORMAT_FLAG_SCIENTIFIC) &&
        !(uint64_t)(fmt & JKN_FF_FORMAT_FLAG_FIXED)) {
      return jkn_ff_report_parse_error(p, missing_exponential_part);
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
    jkn_ff_debug("digit_count %lld\n", digit_count);
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

    jkn_ff_debug("digit_count2 %lld\n", digit_count);
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

#ifdef JKN_FF_DEBUG 

#include <stdio.h>
jkn_ff_internal jkn_ff_inline
void jkn_ff_dump_parsed(jkn_ff_parsed const p) {
  jkn_ff_debug("mantissa: %llu\n", (unsigned long long)p.mantissa);
  jkn_ff_debug("exponent: %lld\n", (long long)p.exponent);
  jkn_ff_debug("negative: %d\n", p.negative);
  jkn_ff_debug("valid: %d\n", p.valid);
  jkn_ff_debug("too_many_digits: %d\n", p.too_many_digits);
  jkn_ff_debug("int_part_len: %zu\n", p.int_part_len);
  jkn_ff_debug("fraction_part_len: %zu\n", p.fraction_part_len);
}

#endif

/* section end: parse */

#endif // JKN_FF_PARSE_H
