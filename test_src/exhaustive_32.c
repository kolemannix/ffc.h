#include "jkn_ff.h"

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define FLOAT_MAXDIGITS_10 9
#define DOUBLE_MAXDIGITS_10 17

char *to_string_long(float d, char *buffer) {
  int written = snprintf(buffer, 128, "%.*e", 64, d);
  return buffer + written;
}

char *to_string(float d, char *buffer) {
  int written = snprintf(buffer, 64, "%.*e", FLOAT_MAXDIGITS_10 - 1, d);
  return buffer + written;
}

void allvalues(char *buffer, bool do_long) {
  for (uint64_t w = 0; w <= 0xFFFFFFFF; w++) {
    float v;
    if ((w % 1048576) == 0) {
      fputc('.', stdout);
      fflush(stdout);
    }
    uint32_t word = (uint32_t)(w);
    memcpy(&v, &word, sizeof(v));

    {
      char const *string_end = do_long ? to_string(v, buffer) : to_string_long(v, buffer);
      float result_value;
      jkn_ff_result result = jkn_ff_from_chars_float(buffer, string_end, &result_value);
      // Starting with version 4.0 for fast_float, we return result_out_of_range
      // if the value is either too small (too close to zero) or too large
      // (effectively infinity). So std::errc::result_out_of_range is normal for
      // well-formed input strings.
      if (result.outcome != JKN_FF_OUTCOME_OK &&
          result.outcome != JKN_FF_OUTCOME_OUT_OF_RANGE) {
        fprintf(stderr, "parsing error ? %s\n", buffer);
        abort();
      }
      if (isnan(v)) {
        if (!isnan(result_value)) {
          fprintf(stderr, "not nan %s\n", buffer);
          abort();
        }
      } else if (copysign(1, result_value) != copysign(1, v)) {
        fprintf(stderr, "%s\n", buffer);
        fprintf(stderr, "I got 0x%a but I was expecting %a\n", result_value, v);
        abort();
      } else if (result_value != v) {
        fprintf(stderr, "no match ? %s got %f expected %f\n", buffer, result_value, v);
        fprintf(stderr, "started with 0x%a\n", v);
        fprintf(stderr, "got back 0x%a\n", result_value);
        abort();
      }
    }
  }
  puts("");
}

int exhaustive_32_run() {
  char buffer[64];
  char buffer_long[128];
  allvalues(buffer, false);
  allvalues(buffer_long, true);
  puts("\nall ok");
  return 0;
}
