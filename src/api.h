#ifndef FFC_API
#define FFC_API

#define FFC_VERSION_YEAR  26
#define FFC_VERSION_MONTH 02
#define FFC_VERSION_BUILD 01
#define FFC_VERSION ((FFC_VERSION_YEAR << 16) | (FFC_VERSION_MONTH << 8) | (FFC_VERSION_BUILD))

#include <stddef.h>
#include <stdint.h>

typedef uint32_t ffc_outcome;
enum ffc_outcome_bits {
  FFC_OUTCOME_OK = 0,
  FFC_OUTCOME_INVALID_INPUT = 1,
  FFC_OUTCOME_OUT_OF_RANGE = 2,
};

typedef struct ffc_result {
  // Where parsing stopped
  char const *ptr;
  // The outcome of the call
  ffc_outcome outcome;
} ffc_result;

typedef uint64_t ffc_format;
enum ffc_format_bits {
  FFC_FORMAT_FLAG_SCIENTIFIC         = 1ULL << 0,
  FFC_FORMAT_FLAG_FIXED              = 1ULL << 2, // Gap is present in fast_float original
  FFC_FORMAT_FLAG_HEX                = 1ULL << 3,
  FFC_FORMAT_FLAG_NO_INFNAN          = 1ULL << 4,
  FFC_FORMAT_FLAG_BASIC_JSON         = 1ULL << 5,
  FFC_FORMAT_FLAG_BASIC_FORTRAN      = 1ULL << 6,
  FFC_FORMAT_FLAG_ALLOW_LEADING_PLUS = 1ULL << 7,
  FFC_FORMAT_FLAG_SKIP_WHITE_SPACE   = 1ULL << 8,

  /* Presets */
  FFC_PRESET_GENERAL = FFC_FORMAT_FLAG_FIXED | FFC_FORMAT_FLAG_SCIENTIFIC,
  
  FFC_PRESET_JSON = FFC_FORMAT_FLAG_BASIC_JSON | 
                       FFC_PRESET_GENERAL | 
                       FFC_FORMAT_FLAG_NO_INFNAN,

  FFC_PRESET_JSON_OR_INFNAN = FFC_FORMAT_FLAG_BASIC_JSON | 
                                 FFC_PRESET_GENERAL,

  FFC_PRESET_FORTRAN = FFC_FORMAT_FLAG_BASIC_FORTRAN | 
                          FFC_PRESET_GENERAL
};

typedef struct ffc_parse_options {
  /** Which number formats are accepted */
  ffc_format format;
  /** The character used as decimal point; period will be used if decimal_point == '\0' */
  char decimal_point;
} ffc_parse_options;

ffc_parse_options ffc_parse_options_default(void);

typedef enum ffc_parse_outcome {
  FFC_PARSE_OUTCOME_NO_ERROR = 0,
  // [JSON-only] The minus sign must be followed by an integer.
  FFC_PARSE_OUTCOME_JSON_MISSING_INTEGER_AFTER_SIGN = 1,
  // A sign must be followed by an integer or dot.
  FFC_PARSE_OUTCOME_MISSING_INTEGER_OR_DOT_AFTER_SIGN = 2,
  // [JSON-only] The integer part must not have leading zeros.
  FFC_PARSE_OUTCOME_JSON_LEADING_ZEROS_IN_INTEGER_PART = 3,
  // [JSON-only] The integer part must have at least one digit.
  FFC_PARSE_OUTCOME_JSON_NO_DIGITS_IN_INTEGER_PART = 4,
  // [JSON-only] If there is a decimal point, there must be digits in the
  // fractional part.
  FFC_PARSE_OUTCOME_JSON_NO_DIGITS_IN_FRACTIONAL_PART = 5,
  // The mantissa must have at least one digit.
  FFC_PARSE_OUTCOME_NO_DIGITS_IN_MANTISSA = 6,
  // Scientific notation requires an exponential part.
  FFC_PARSE_OUTCOME_MISSING_EXPONENTIAL_PART = 7,
} ffc_parse_outcome;

/*
 * A simplified API for simple use cases; the result will be 0.0 on error, not uninitialized.
 * If outcome is null, it will not be written to
 */
double ffc_parse_double_simple(size_t len, const char *s, ffc_outcome *outcome);
ffc_result ffc_parse_double(size_t len, const char *s, double *out);
/**
 * Implements the fast_float algorithm from https://github.com/fastfloat/fast_float
 * See original for more details
 *
 * This function parses the character sequence [first,last) for a number. It
 * parses floating-point numbers expecting a locale-independent format equivalent
 * to what is used by std::strtod in the default ("C") locale. The resulting
 * floating-point value is the closest floating-point value (using either float
 * or double), using the "round to even" convention for values that would
 * otherwise fall right in-between two values. That is, we provide exact parsing
 * according to the IEEE standard.
 *
 * Given a successful parse, the pointer (`ptr`) in the returned value is set to
 * point right after the parsed number, and the `value` referenced is set to the
 * parsed value. In case of error, the returned `ec` contains a representative
 * error, otherwise the default (`FFC_OUTCOME_OK`) value is stored.
 *
 * The implementation does not allocate heap memory.
 *
 * Like the C++17 standard, the `fast_float::from_chars` functions take an
 * optional last argument of the type `fast_float::chars_format`. It is a bitset
 * value: we check whether `fmt & fast_float::chars_format::fixed` and `fmt &
 * fast_float::chars_format::scientific` are set to determine whether we allow
 * the fixed point and scientific notation respectively. The default is
 * `fast_float::chars_format::general` which allows both `fixed` and
 * `scientific`.
 */
ffc_result ffc_from_chars_double(const char *start, const char *end, double* out);
ffc_result ffc_from_chars_double_options(const char *start, const char *end, double* out, ffc_parse_options options);

/*
 * A simplified API for simple use cases; the result will be 0.0 on error, not uninitialized.
 * If outcome is null, it will not be written to
 */
float ffc_parse_float_simple(size_t len, const char *s, ffc_outcome *outcome);
ffc_result ffc_parse_float(size_t len, const char *s, float *out);
ffc_result ffc_from_chars_float(const char *start,  const char *end, float* out);
ffc_result ffc_from_chars_float_options(const char *start,  const char *end, float* out, ffc_parse_options options);

ffc_result ffc_parse_i64(size_t len, const char input[len], int base, int64_t  *out);
ffc_result ffc_parse_u64(size_t len, const char input[len], int base, uint64_t *out);
ffc_result ffc_parse_i32(size_t len, const char input[len], int base, int32_t  *out);
ffc_result ffc_parse_u32(size_t len, const char input[len], int base, uint32_t *out);
// nocommit implement more integer overloads
// ffc_result ffc_from_chars_long(const char *start, const char *end, long* out);
// ffc_result ffc_from_chars_int(const char *start,  const char *end, int* out);

#endif // FFC_API
