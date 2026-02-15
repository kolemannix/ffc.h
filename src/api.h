#ifndef JKN_FF_API
#define JKN_FF_API

#include <stddef.h>
#include <stdint.h>

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
  /** The character used as decimal point; period will be used if decimal_point == '\0' */
  char decimal_point;
  /** The base used only for integers */
  int base;
} jkn_ff_parse_options;

jkn_ff_parse_options jkn_ff_parse_options_default();

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

// nocommit: restrict since its char*?
jkn_ff_result jkn_ff_parse_double(size_t len, const char input[len], double* out);
jkn_ff_result jkn_ff_parse_float(size_t len, const char input[len], float* out);

jkn_ff_result jkn_ff_from_chars_double(const char *start, const char *end, float* out);
jkn_ff_result jkn_ff_from_chars_float(const char *start,  const char *end, double* out);

#endif // JKN_FF_API
