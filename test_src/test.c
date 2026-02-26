#define SONICSV_IMPLEMENTATION
#include "sonicsv.h"

#define JKN_FF_DEBUG 0
#include "jkn_ff.h"
#include <stdlib.h>
#include <stdio.h>
#include <fenv.h>
#include "assert.h"
#include <inttypes.h>

#define FLOAT_MAXDIGITS_10 9
#define DOUBLE_MAXDIGITS_10 17

char *float_to_string_long(float f, char *buffer) {
  int written = snprintf(buffer, 128, "%.*e", 64, f);
  return buffer + written;
}

char *float_to_string(float f, char *buffer) {
  int written = snprintf(buffer, 64, "%.*e", FLOAT_MAXDIGITS_10 - 1, f);
  return buffer + written;
}

char *double_to_string_long(double d, char *buffer) {
  int written = snprintf(buffer, 128, "%.*e", 64, d);
  return buffer + written;
}

char *double_to_string(double d, char *buffer) {
  int written = snprintf(buffer, 64, "%.*e", FLOAT_MAXDIGITS_10 - 1, d);
  return buffer + written;
}

inline jkn_ff_outcome parse_outcome(uint64_t len, const char* outcome_text) {
    static const struct { const char *name; jkn_ff_outcome val; } map[] = {
        {"ok",           JKN_FF_OUTCOME_OK},
        {"out_of_range", JKN_FF_OUTCOME_OUT_OF_RANGE},
        {"invalid",      JKN_FF_OUTCOME_INVALID_INPUT},
    };
    if (len < strlen(map[0].name)) {
      fprintf(stderr, "unexpected outcome text: '%.*s'\n", (int)len, outcome_text);
      return JKN_FF_OUTCOME_OK;
    }
    for (size_t i = 0; i < sizeof(map)/sizeof(*map); i++) {
        if (strncmp(outcome_text, map[i].name, (size_t)len) == 0) return map[i].val;
    }
    fprintf(stderr, "unexpected outcome text: '%.*s'\n", (int)len, outcome_text);
    return JKN_FF_OUTCOME_OK;
}

static inline char* get_outcome_name(jkn_ff_outcome outcome) {
  switch (outcome) {
  case JKN_FF_OUTCOME_OK: return "ok";
  case JKN_FF_OUTCOME_INVALID_INPUT: return "invalid";
  case JKN_FF_OUTCOME_OUT_OF_RANGE: return "out_of_range";
  default: return "unknown";
  }
}

static inline char* my_strndup(size_t len, const char* src) {
  char *nt = malloc(len + 1);
  memcpy(nt, src, len);
  nt[len] = '\0';
  return nt;
}

char const *round_name(int d) {
  switch (d) {
  case FE_UPWARD:
    return "FE_UPWARD";
  case FE_DOWNWARD:
    return "FE_DOWNWARD";
  case FE_TOWARDZERO:
    return "FE_TOWARDZERO";
  case FE_TONEAREST:
    return "FE_TONEAREST";
  default:
    return "UNKNOWN";
  }
}

char *fHexAndDec_double(double v) {
    char dec[64];
    char *ret = malloc(128);
    snprintf(dec, sizeof(dec), "%.*g", DBL_MAX_10_EXP + 1, v);
    snprintf(ret, 128, "%a (%s)", v, dec);
    return ret;
}

static int FAILS = 0;

bool double_eq(double exp, double act) {
  if (exp == act) {
    return true;
  } else {
    if (isnan(exp) && isnan(act)) {
      return signbit(exp) == signbit(act);
    } else {
      return false;
    }
  };
}

bool float_eq(float exp, float act) {
  if (exp == act) {
    return true;
  } else {
    if (isnan(exp) && isnan(act)) {
      return signbit(exp) == signbit(act);
    } else {
      return false;
    }
  };
}

void assert_double(size_t len, char *input, double exp, double act) {
  if (!double_eq(exp, act)) {
    printf("\n\ninput: %.*s\n", (int)len, input);
    printf("\texp: %f\n\tact: %f\n\n", exp, act);
    printf("\texp bits: 0x%"PRIx64"\n\tact bits: 0x%"PRIx64"\n\n", ffc_get_double_bits(exp), ffc_get_double_bits(act));
    FAILS += 1;
  }
}

void assert_float(size_t len, char *input, float exp, float act) {
  if (!float_eq(exp, act)) {
    printf("\n\ninput: %.*s\n", (int)len, input);
    printf("\texp: %f\n\tact: %f\n\n", exp, act);
    printf("\texp bits: 0x%x\n\tact bits: 0x%x\n\n", ffc_get_float_bits(exp), ffc_get_float_bits(act));
    FAILS += 1;
  }
}

void verify_ext(size_t len, char input[len], ffc_value exp_value, ffc_value_kind vk, jkn_ff_outcome exp_outcome, jkn_ff_parse_options options) {
  ffc_value value;

  jkn_ff_result result = jkn_ff_from_chars(input, &input[len], options, &value, vk);

  if (exp_outcome != result.outcome) {
    printf("\n\ninput: %.*s\n", (int)len, input);
    printf("\tFAIL expected %s actual %s\n", get_outcome_name(exp_outcome), get_outcome_name(result.outcome)); 
    FAILS += 1;
  }

  if (exp_outcome == JKN_FF_OUTCOME_OK) {
    switch (vk) {
    case FFC_VALUE_KIND_DOUBLE:
      assert_double(len, input, exp_value.d, value.d);
      break;

    case FFC_VALUE_KIND_FLOAT:
      assert_float(len, input, exp_value.f, value.f);
      break;
    }
    
  }
}

void verify_double_ext(size_t len, char input[len], double exp_value, jkn_ff_outcome exp_outcome, jkn_ff_parse_options options) {
  ffc_value expected;
  expected.d = exp_value;
  return verify_ext(len, input, expected, FFC_VALUE_KIND_DOUBLE, exp_outcome, options);
}

void verify_float_ext(size_t len, char input[len], float exp_value, jkn_ff_outcome exp_outcome, jkn_ff_parse_options options) {
  ffc_value expected;
  expected.f = exp_value;
  return verify_ext(len, input, expected, FFC_VALUE_KIND_FLOAT, exp_outcome, options);
}

void verify_float(char *input, float exp_value) {
  return verify_float_ext(strlen(input), input, exp_value, JKN_FF_OUTCOME_OK, jkn_ff_parse_options_default());
}

jkn_ff_result run_double_options(char *input, double *out, jkn_ff_parse_options options) {
  size_t len = strlen(input);
  return jkn_ff_from_chars_double_options(input, &input[len], out, options);
}

#define verify(input, value) verify_double_ext(strlen(input), input, value, JKN_FF_OUTCOME_OK, jkn_ff_parse_options_default())
#define verify_oor(input, value) verify_double_ext(strlen(input), input, value, JKN_FF_OUTCOME_OUT_OF_RANGE, jkn_ff_parse_options_default())
#define verify_err(input, value, outcome) verify_double_ext(strlen(input), input, value, outcome, jkn_ff_parse_options_default())
#define verify_options(input, value, outcome) verify_double_ext(strlen(input), input, value, outcome, options)

double const DBL_INF = (double)INFINITY;

// Assumes C strings
// Caller owns returned string
char *append_zeros(const char *str, size_t number_of_zeros) {
  size_t len = strlen(str);
  char *answer = malloc(len + number_of_zeros + 1);
  memcpy(answer, str, len);
  memset(answer + len, '0', number_of_zeros);
  answer[len + number_of_zeros] = '\0';
  return answer;
}

// Assumes C strings
char *string_concat(const char *str, const char *other) {
  size_t l1 = strlen(str);
  size_t l2 = strlen(other);
  size_t len = l1 + l2;
  char *result = malloc(len + 1);
  memcpy(result, str, l1);
  memcpy(result + l1, other, l2);
  result[len] = '\0';
  return result;
}

void double_special(void) {
  verify(append_zeros("9007199254740993.0", 1000), 0x1p+53);

  struct test_case {
    char* input_data;
    bool expected_success;
    double expected_result;
  };
  const struct test_case whitespace_tests[] = {
    //whitespace stresstest
    {" \r\n\t\f\v3.16227766016838 \r\n\t\f\v", true, 3.16227766016838},
    {" \r\n\t\f\v3 \r\n\t\f\v", true, 3.0},
    {" \r\n\t\f\v2.82842712474619 \r\n\t\f\v", true, 2.82842712474619},
    {" \r\n\t\f\v2.44948974278318 \r\n\t\f\v", true, 2.44948974278318},
    {" \r\n\t\f\v2 \r\n\t\f\v", true, 2.0},
    {" \r\n\t\f\v0 \r\n\t\f\v", true, 0.0},
    {" \r\n\t\f\v1.73205080756888 \r\n\t\f\v", true, 1.73205080756888},
    {" \r\n\t\f\v1 \r\n\t\f\v", true, 1.0},
    {" \r\n\t\f\v1.4142135623731 \r\n\t\f\v", true, 1.4142135623731},
    {" \r\n\t\f\v2.23606797749979 \r\n\t\f\v", true, 2.23606797749979},
    {" \r\n\t\f\v2.64575131106459 \r\n\t\f\v", true, 2.64575131106459},
    {"+2.2",true,2.2 },
    {"0.",true,0.0 },
    {"-.1",true,-0.1 },
    {"+.1",true,0.1 },
    {"1e+1",true,10.0 },
    {"+1e1",true,10.0 },
    {"-+0",false,0.0 }
  };

  jkn_ff_parse_options options = jkn_ff_parse_options_default();
  options.format |= JKN_FF_FORMAT_FLAG_SKIP_WHITE_SPACE;
  options.format |= JKN_FF_FORMAT_FLAG_ALLOW_LEADING_PLUS;

  for (size_t i = 0; i < sizeof(whitespace_tests)/sizeof(*whitespace_tests); i++) {
    const struct test_case *test_data = &whitespace_tests[i];
    jkn_ff_outcome exp_outcome = test_data->expected_success ? JKN_FF_OUTCOME_OK : JKN_FF_OUTCOME_INVALID_INPUT;
    verify_double_ext(
      strlen(test_data->input_data),
      test_data->input_data,
      test_data->expected_result,
      exp_outcome,
      options
    );
  }

  // {"1d+4",false,0.0 },
  double out;
  char *i1 = "1d+4";
  jkn_ff_result r = run_double_options(i1, &out, options);
  ptrdiff_t len = r.ptr - i1;
  assert(len == 1);
  assert(r.outcome == JKN_FF_OUTCOME_OK);
  assert(out == 1.0);

  // {"1d-1",false,0.0 },
  i1 = "1d-1";
  r = run_double_options(i1, &out, options);
  len = r.ptr - i1;
  assert(len == 1);
  assert(r.outcome == JKN_FF_OUTCOME_OK);
  assert(out == 1.0);

}

typedef struct cb_test_context {
  jkn_ff_parse_options options;
  ffc_value_kind value_kind;
} cb_test_context;

void cb_test(const csv_row_t *row, void *ctx) {
  // Extract settings from context
  if (!ctx) {
    fprintf(stderr, "cb_test missing ctx; aborting\n");
    abort();
  }
  cb_test_context *test_ctx = (cb_test_context*)ctx;
  jkn_ff_parse_options options;
  ffc_value_kind vk = test_ctx->value_kind;
  options = test_ctx->options;

  if (row->num_fields < 2) {
    fprintf(stderr, "ERROR test row %d has only %zu column\n", (int)row->row_number, row->num_fields);
    FAILS += 1;
    return;
  }
  if (row->row_number == 1) {
    return;
  }
  // Required
  const csv_field_t *input_field = csv_get_field(row, 0);
  const csv_field_t *expected_field = csv_get_field(row, 1);

  // Possibly missing
  const csv_field_t *outcome_field = csv_get_field(row, 2);
  const csv_field_t *comment = csv_get_field(row, 3);
  (void)comment;

  ffc_value expected_value; 
  bool is_max = strncmp(expected_field->data, "MAX", 3) == 0;
  bool is_neg_max = strncmp(expected_field->data, "-MAX", 4) == 0;
  bool is_min = strncmp(expected_field->data, "MIN", 3) == 0;
  bool is_neg_min = strncmp(expected_field->data, "-MIN", 4) == 0;
  switch (vk) {
  case FFC_VALUE_KIND_DOUBLE:
    if (is_max) {
      expected_value.d = DBL_MAX;
    } else if (is_neg_max) {
      expected_value.d = -DBL_MAX;
    } else if (is_min) {
      expected_value.d = DBL_MIN;
    } else if (is_neg_min) {
      expected_value.d = -DBL_MIN;
    } else {
      expected_value.d = strtod(my_strndup(expected_field->size, expected_field->data), NULL);
    };
    break;
  case FFC_VALUE_KIND_FLOAT:
    if (is_max) {
      expected_value.f = FLT_MAX;
    } else if (is_neg_max) {
      expected_value.f = -FLT_MAX;
    } else if (is_min) {
      expected_value.f = FLT_MIN;
    } else if (is_neg_min) {
      expected_value.f = -FLT_MIN;
    } else {
      expected_value.f = strtof(my_strndup(expected_field->size, expected_field->data), NULL);
    };
    break;
  }
  jkn_ff_outcome exp_outcome = outcome_field ? 
    parse_outcome(outcome_field->size, outcome_field->data) : JKN_FF_OUTCOME_OK;

  verify_ext(input_field->size, (char*)input_field->data, expected_value, vk, exp_outcome, options);
  return;
}

void test_file(const char* filename, csv_row_callback_t cb, jkn_ff_parse_options options, ffc_value_kind vk) {
    csv_parser_t *p = csv_parser_create(NULL);
    cb_test_context test_ctx = {0};
    test_ctx.options = options;
    test_ctx.value_kind = vk;
    csv_parser_set_row_callback(p, cb, &test_ctx);
    csv_error_t error = csv_parse_file(p, filename);
    if (error != CSV_OK) {
      fprintf(stderr, "Failed to load test file: %s\n", filename);
      abort();
    }
    csv_parser_destroy(p);
}

jkn_ff_result roundtrip_float(float f, float *result, bool do_long) {
  char buffer[128];
  char const *string_end = do_long ? float_to_string_long(f, buffer) : float_to_string(f, buffer);
  jkn_ff_result parse_result = jkn_ff_from_chars_float(buffer, string_end, result);
  return parse_result;
}

void test_exhaustive(char *buffer, bool do_long) {
  for (uint64_t w = 0; w <= 0xFFFFFFFF; w++) {
    float f;
    double d;
    if ((w % 1048576) == 0) {
      printf("%"PRIu64" / %"PRIu32"\n", w / 1048576, 0xFFFFFFFF / 1048576);
    }
    uint32_t word32 = (uint32_t)(w);

    memcpy(&f, &word32, sizeof(f));
    memcpy(&d, &w, sizeof(d));

#if 1
    {
      char const *string_end = do_long ? float_to_string_long(f, buffer) : float_to_string(f, buffer);
      float result_value;
      jkn_ff_result result = jkn_ff_from_chars_float(buffer, string_end, &result_value);
      // Starting with version 4.0 for fast_float, we return result_out_of_range
      // if the value is either too small (too close to zero) or too large
      // (effectively infinity). So std::errc::result_out_of_range is normal for
      // well-formed input strings.
      if (result.outcome != JKN_FF_OUTCOME_OK &&
          result.outcome != JKN_FF_OUTCOME_OUT_OF_RANGE) {
        fprintf(stderr, "(32) parsing error ? %s\n", buffer);
        abort();
      }
      if (isnan(f)) {
        if (!isnan(result_value)) {
          fprintf(stderr, "(32) not nan %s\n", buffer);
          abort();
        }
      } else if (copysign(1, result_value) != copysign(1, f)) {
        fprintf(stderr, "(32) %s\n", buffer);
        fprintf(stderr, "(32) I got %a but I was expecting %a\n", result_value, f);
        abort();
      } else if (result_value != f) {
        fprintf(stderr, "(32) fail for w = %"PRIu64"\n%s got %f expected %f\n", w, buffer, result_value, f);
        fprintf(stderr, "(32) started with %a\n", f);
        fprintf(stderr, "(32) got back     %a\n", result_value);
        abort();
      }
    }
#endif
    {
      char const *string_end = do_long ? double_to_string_long(d, buffer) : double_to_string(d, buffer);
      double result_double;
      jkn_ff_result result = jkn_ff_from_chars_double(buffer, string_end, &result_double);
      // Starting with version 4.0 for fast_float, we return result_out_of_range
      // if the value is either too small (too close to zero) or too large
      // (effectively infinity). So std::errc::result_out_of_range is normal for
      // well-formed input strings.
      if (result.outcome != JKN_FF_OUTCOME_OK &&
          result.outcome != JKN_FF_OUTCOME_OUT_OF_RANGE) {
        fprintf(stderr, "(64) parsing error ? %s\n", buffer);
        abort();
      }
      if (!double_eq(result_double, d)) {
        fprintf(stderr, "(64) fail for w = %"PRIu64"\n%s got %f expected %f\n", w, buffer, result_double, d);
        fprintf(stderr, "(64) started with %a\n", d);
        fprintf(stderr, "(64) got back     %a\n", result_double);
        abort();
      }
    }
  }
  puts("");
}

void exhaustive_32_run(void) {
  char buffer_long[128];
  test_exhaustive(buffer_long, true);
  puts("\nall ok");
  return;
}


void double_rounds_to_nearest(void) {
 static volatile double fmin = DBL_MIN;
    char *s1, *s2;

    fesetround(FE_UPWARD);
    s1 = fHexAndDec_double(fmin + 1.0);
    s2 = fHexAndDec_double(1.0 - fmin);
    free(s1); free(s2);
    assert(fegetround() == FE_UPWARD);
    assert(jkn_ff_rounds_to_nearest() == false);

    fesetround(FE_DOWNWARD);
    s1 = fHexAndDec_double(fmin + 1.0);
    s2 = fHexAndDec_double(1.0 - fmin);
    free(s1); free(s2);
    assert(fegetround() == FE_DOWNWARD);
    assert(jkn_ff_rounds_to_nearest() == false);

    fesetround(FE_TOWARDZERO);
    s1 = fHexAndDec_double(fmin + 1.0);
    s2 = fHexAndDec_double(1.0 - fmin);
    free(s1); free(s2);
    assert(fegetround() == FE_TOWARDZERO);
    assert(jkn_ff_rounds_to_nearest() == false);

    fesetround(FE_TONEAREST);
    s1 = fHexAndDec_double(fmin + 1.0);
    s2 = fHexAndDec_double(1.0 - fmin);
    free(s1); free(s2);
    assert(fegetround() == FE_TONEAREST);
#if (FLT_EVAL_METHOD == 1) || (FLT_EVAL_METHOD == 0)
    assert(jkn_ff_rounds_to_nearest() == true);
#endif
}

void double_parse_zero(void) {
  //
  // If this function fails, we may be left in a non-standard rounding state.
  //
  char const *zero = "0";
  uint64_t float64_parsed;
  double f = 0;
  memcpy(&float64_parsed, &f, sizeof(f));
  assert(float64_parsed == 0);

  fesetround(FE_UPWARD);
  jkn_ff_result r1 = jkn_ff_from_chars_double(zero, zero + 1, &f);
  assert(r1.outcome == JKN_FF_OUTCOME_OK);
  assert(f == 0.);
  memcpy(&float64_parsed, &f, sizeof(f));
  assert(float64_parsed == 0);

  fesetround(FE_TOWARDZERO);
  jkn_ff_result r2 = jkn_ff_from_chars_double(zero, zero + 1, &f);
  assert(r2.outcome == JKN_FF_OUTCOME_OK);
  assert(f == 0.);
  memcpy(&float64_parsed, &f, sizeof(f));
  assert(float64_parsed == 0);

  fesetround(FE_DOWNWARD);
  jkn_ff_result r3 = jkn_ff_from_chars_double(zero, zero + 1, &f);
  assert(r3.outcome == JKN_FF_OUTCOME_OK);
  assert(f == 0.);
  memcpy(&float64_parsed, &f, sizeof(f));
  assert(float64_parsed == 0);

  fesetround(FE_TONEAREST);
  jkn_ff_result r4 = jkn_ff_from_chars_double(zero, zero + 1, &f);
  assert(r4.outcome == JKN_FF_OUTCOME_OK);
  assert(f == 0.);
  memcpy(&float64_parsed, &f, sizeof(f));
  assert(float64_parsed == 0);
}

void double_parse_negative_zero(void) {
  //
  // If this function fails, we may be left in a non-standard rounding state.
  //
  char const *negative_zero = "-0";
  double f = -0.;
  assert(ffc_get_double_bits(f) == 0x8000000000000000ULL);

  fesetround(FE_UPWARD);
  jkn_ff_result r1 = jkn_ff_from_chars_double(negative_zero, negative_zero + 2, &f);
  assert(r1.outcome == JKN_FF_OUTCOME_OK);
  char *s1 = fHexAndDec_double(f);
  free(s1);
  assert(f == 0.);
  assert(ffc_get_double_bits(f) == 0x8000000000000000ULL);

  fesetround(FE_TOWARDZERO);
  jkn_ff_result r2 = jkn_ff_from_chars_double(negative_zero, negative_zero + 2, &f);
  assert(r2.outcome == JKN_FF_OUTCOME_OK);
  char *s2 = fHexAndDec_double(f);
  free(s2);
  assert(f == 0.);
  assert(ffc_get_double_bits(f) == 0x8000000000000000ULL);

  fesetround(FE_DOWNWARD);
  jkn_ff_result r3 = jkn_ff_from_chars_double(negative_zero, negative_zero + 2, &f);
  assert(r3.outcome == JKN_FF_OUTCOME_OK);
  char *s3 = fHexAndDec_double(f);
  free(s3);
  assert(f == 0.);
  assert(ffc_get_double_bits(f) == 0x8000000000000000ULL);

  fesetround(FE_TONEAREST);
  jkn_ff_result r4 = jkn_ff_from_chars_double(negative_zero, negative_zero + 2, &f);
  assert(r4.outcome == JKN_FF_OUTCOME_OK);
  char *s4 = fHexAndDec_double(f);
  free(s4);
  assert(f == 0.);
  assert(ffc_get_double_bits(f) == 0x8000000000000000ULL);
}

void float_special() {
  verify_float(append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 655), 0x1.2ced3p+0f);
  verify_float(append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 656), 0x1.2ced3p+0f);
  verify_float(
    append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 1000),
    0x1.2ced3p+0f
  );
  char *test_string;
  test_string = string_concat(
    append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 655),
    "e-38"
  );
  verify_float(test_string, 0x1.fffff8p-127f);
  test_string = string_concat(
    append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 656),
    "e-38"
  );
  verify_float(test_string, 0x1.fffff8p-127f),
  test_string = string_concat(
    append_zeros("1.1754941406275178592461758986628081843312458647327962400313859427181746759860647699724722770042717456817626953125", 1000),
    "e-38"
  );
  verify_float(test_string, 0x1.fffff8p-127f);

  verify_float("1.00000006e+09", 1.00000006e+09f);
  verify_float("1.4012984643e-45f", 1.4012984643e-45f);
  verify_float("1.1754942107e-38f", 1.1754942107e-38f);
  verify_float("1.1754943508e-45f", 1.1754943508e-45f);
}

int main(void) {

  // verify_float("1.1754942807573642917e-38", 0x1.fffffcp-127f);
  // exit(0);

  /*
   * We store our test cases in csv files of the format:
   * input,expected,code,comment
   * 
   * We use strtod/strtof to parse 'expected', we compare bit-for-bit with our result
   * Valid values for 'code' are determined by `parse_outcome`: "ok","out_of_range","invalid"
   */

  jkn_ff_parse_options opts = jkn_ff_parse_options_default();
  jkn_ff_parse_options comma = opts;
  comma.decimal_point = ',';
  test_file("test_src/double_cases_general.csv", &cb_test, opts, FFC_VALUE_KIND_DOUBLE);
  test_file("test_src/double_cases_infnan.csv", &cb_test, opts, FFC_VALUE_KIND_DOUBLE);
  test_file("test_src/double_cases_comma.csv", &cb_test, comma, FFC_VALUE_KIND_DOUBLE);

  test_file("test_src/float_cases.csv", &cb_test, opts, FFC_VALUE_KIND_FLOAT);

  double_special();
  double_rounds_to_nearest();
  double_parse_zero();
  double_parse_negative_zero();

  float_special();

  #if FFC_TEST_EXHAUSTIVE
  exhaustive_32_run();
  #endif

  if (FAILS != 0) {
    fprintf(stderr, "Test failures from csvs: %d\n", FAILS);
    return 1;
  }

  return 0;
}

