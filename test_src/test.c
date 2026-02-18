#define JKN_FF_DEBUG 0
#include "jkn_ff.h"
#include <stdlib.h>
#include <stdio.h>

#define SONICSV_IMPLEMENTATION
#include "sonicsv.h"

inline jkn_ff_outcome parse_outcome(uint64_t len, const char* outcome_text) {
    static const struct { const char *name; jkn_ff_outcome val; } map[] = {
        {"ok",           JKN_FF_OUTCOME_OK},
        {"out_of_range", JKN_FF_OUTCOME_OUT_OF_RANGE},
        {"invalid",      JKN_FF_OUTCOME_INVALID_INPUT},
    };
    if (len < strlen(map[0].name)) {
      fprintf(stderr, "unexpected outcome text: '%s'\n", outcome_text);
      return JKN_FF_OUTCOME_OK;
    }
    for (size_t i = 0; i < sizeof(map)/sizeof(*map); i++) {
        if (strncmp(outcome_text, map[i].name, (size_t)len) == 0) return map[i].val;
    }
    fprintf(stderr, "unexpected outcome text: '%s'\n", outcome_text);
    return JKN_FF_OUTCOME_OK;
}

inline char* get_outcome_name(jkn_ff_outcome outcome) {
  switch (outcome) {
  case JKN_FF_OUTCOME_OK: return "ok";
  case JKN_FF_OUTCOME_INVALID_INPUT: return "invalid";
  case JKN_FF_OUTCOME_OUT_OF_RANGE: return "out_of_range";
  default: return "unknown";
  }
}

inline char* my_strndup(size_t len, const char* src) {
  char *nt = malloc(len + 1);
  memcpy(nt, src, len);
  nt[len] = '\0';
  return nt;
}

static int FAILS = 0;

void verify_double_ext(size_t len, char input[len], double exp_value, jkn_ff_outcome exp_outcome, jkn_ff_parse_options options) {
  double value;

  jkn_ff_result result = jkn_ff_from_chars_double_options(input, &input[len], &value, options);

  if (exp_outcome != result.outcome) {
    printf("\n\ninput: %.*s\n", (int)len, input);
    printf("\tFAIL expected %s actual %s\n", get_outcome_name(exp_outcome), get_outcome_name(result.outcome)); 
    FAILS += 1;
  }

  if (exp_outcome == JKN_FF_OUTCOME_OK) {
    if (exp_value != value) {
      printf("\n\ninput: %.*s\n", (int)len, input);
      printf("\texp: %f\n\tact: %f\n\n", exp_value, value);
      uint64_t exp_bits;
      uint64_t act_bits;
      memcpy(&exp_bits, &exp_value, sizeof(double));
      memcpy(&act_bits, &value, sizeof(double));
      printf("\texp: 0x%llx\n\tact: 0x%llx\n\n", exp_bits, act_bits);
      FAILS += 1;
    }
  }
}

#define verify(input, value) verify_double_ext(strlen(input), input, value, JKN_FF_OUTCOME_OK, jkn_ff_parse_options_default())
#define verify_oor(input, value) verify_double_ext(strlen(input), input, value, JKN_FF_OUTCOME_OUT_OF_RANGE, jkn_ff_parse_options_default())
#define verify_err(input, value, outcome) verify_double_ext(strlen(input), input, value, outcome, jkn_ff_parse_options_default())
#define verify_options(input, value, outcome) verify_double_ext(strlen(input), input, value, outcome, options)

double const DBL_INF = (double)INFINITY;

// Caller owns returned string
char *append_zeros(const char *str, size_t number_of_zeros) {
  size_t len = strlen(str);
  char *answer = malloc(len + number_of_zeros + 1);
  memcpy(answer, str, len);
  memset(answer + len, '0', number_of_zeros);
  answer[len + number_of_zeros] = '\0';
  return answer;
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
  };

  jkn_ff_parse_options options = jkn_ff_parse_options_default();
  options.format |= JKN_FF_FORMAT_FLAG_SKIP_WHITE_SPACE;

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

}

typedef struct cb_test_context {
  jkn_ff_parse_options options;
} cb_test_context;

void cb_test_double(const csv_row_t *row, void *ctx) {
  if (row->num_fields < 2) {
    fprintf(stderr, "Failed to parse test row %lld", row->row_number);
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

  double exp_value; 
  if (strncmp(expected_field->data, "MAX", 3) == 0) {
    exp_value = DBL_MAX;
  } else if (strncmp(expected_field->data, "-MAX", 4) == 0) {
    exp_value = -DBL_MAX;
  } else {
    exp_value = strtod(my_strndup(expected_field->size, expected_field->data), NULL);
  };
  jkn_ff_outcome exp_outcome = outcome_field ? 
    parse_outcome(outcome_field->size, outcome_field->data) : JKN_FF_OUTCOME_OK;

  jkn_ff_parse_options options;
  if (ctx) {
    cb_test_context *test_ctx = (cb_test_context*)ctx;
    options = test_ctx->options;
  } else {
    options = jkn_ff_parse_options_default();
  };

  verify_double_ext(input_field->size, (char*)input_field->data, exp_value, exp_outcome, options);

}

void test_file(const char* filename, csv_row_callback_t cb, jkn_ff_parse_options options) {
    csv_parser_t *p = csv_parser_create(NULL);
    cb_test_context test_ctx = {0};
    test_ctx.options = options;
    csv_parser_set_row_callback(p, cb, &test_ctx);
    csv_parse_file(p, filename);
    csv_parser_destroy(p);
}

#define FFC_TEST_EXHAUSTIVE 1
#if FFC_TEST_EXHAUSTIVE
#include "exhaustive_32.c"
#endif

int main(void) {

  /*
   * We store our test cases in csv files of the format:
   * input,expected,code,comment
   * 
   * We use strtod to parse 'expected', we compare bit-for-bit with our result
   * Valid values for 'code' are determined by `parse_outcome`: "ok","out_of_range","invalid"
   */

  jkn_ff_parse_options d = jkn_ff_parse_options_default();
  jkn_ff_parse_options comma = d;
  comma.decimal_point = ',';
  test_file("double_cases_general.csv", &cb_test_double, d);
  test_file("double_cases_infnan.csv", &cb_test_double, d);
  test_file("double_cases_comma.csv", &cb_test_double, comma);

  double_special();

  #if FFC_TEST_EXHAUSTIVE
  exhaustive_32_run();
  #endif

  if (FAILS != 0) {
    return 1;
  }


  return 0;
}

