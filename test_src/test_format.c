// Tests for ffc_format_double_fixed.
//
// The reference is the C library's printf("%.*f"), which on glibc, musl and
// macOS prints the exact binary value with round-half-to-even. Every case is
// therefore a byte-for-byte comparison against snprintf.
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <float.h>
#include <math.h>

#define FFC_DEBUG 0
#define FFC_IMPL
#include "ffc.h"

static int FAILS = 0;
static long CHECKS = 0;

// Enough for "-" + 309 integer digits + "." + 1200 fraction digits.
#define BUF_SIZE 2048

static void check(double v, int places) {
  char expected[BUF_SIZE];
  char actual[BUF_SIZE];
  int elen = snprintf(expected, sizeof expected, "%.*f", places, v);
  size_t alen = ffc_format_double_fixed(actual, sizeof actual, v, places);
  CHECKS++;
  if (elen < 0 || (size_t)elen != alen || memcmp(expected, actual, alen) != 0) {
    actual[alen < sizeof actual ? alen : sizeof actual - 1] = '\0';
    fprintf(stderr, "FAIL %.17g (bits %016llx) places %d\n  expected %s\n  actual   %s\n",
            v, (unsigned long long)ffc_get_double_bits(v), places, expected, actual);
    FAILS++;
  }
}

static void expect_str(double v, int places, const char *expected) {
  char actual[BUF_SIZE];
  size_t alen = ffc_format_double_fixed(actual, sizeof actual, v, places);
  CHECKS++;
  if (alen != strlen(expected) || memcmp(expected, actual, alen) != 0) {
    actual[alen < sizeof actual ? alen : sizeof actual - 1] = '\0';
    fprintf(stderr, "FAIL %.17g places %d\n  expected %s\n  actual   %s\n",
            v, places, expected, actual);
    FAILS++;
  }
}

static double from_bits(uint64_t bits) {
  double d;
  memcpy(&d, &bits, sizeof d);
  return d;
}

/* -- hand-picked cases ------------------------------------------------ */

static void test_edge_cases(void) {
  static const int PLACES[] = {0, 1, 2, 3, 5, 6, 10, 15, 16, 17, 20, 30, 50, 100, 400, 1074, 1100};
  static const double VALUES[] = {
    0.0, 1.0, -1.0, 0.5, 1.5, 2.5, 3.5, -0.5, -2.5,
    0.125, 0.375, 0.625, 0.875, 0.045, 1.005, 2.675, 9.995, 9.9999995, 0.9999995,
    0.1, 0.2, 0.3, 0.7, 1.0 / 3, 2.0 / 3, 123456.789, -123456.789,
    1e-7, 1e-10, 1e-300, 1e15, 1e16, 1e17, 1e22, 1e23, 1e100, 1e300,
    4503599627370496.5, 4503599627370497.5, 9007199254740993.0,
    4294967295.0, 4294967296.0, 18446744073709551615.0, 18446744073709551616.0,
    DBL_MAX, -DBL_MAX, DBL_MIN, DBL_EPSILON, 4.9406564584124654e-324, -4.9406564584124654e-324,
    3.141592653589793, 2.718281828459045, 299792458.0, 6.02214076e23, 1.602176634e-19,
  };
  for (size_t i = 0; i < sizeof VALUES / sizeof *VALUES; i++) {
    for (size_t j = 0; j < sizeof PLACES / sizeof *PLACES; j++) {
      check(VALUES[i], PLACES[j]);
    }
  }
  // Interesting bit patterns: powers of two around the limb boundaries,
  // and the largest subnormal / smallest normal.
  for (int e = -1074; e <= 1023; e += 7) {
    check(ldexp(1.0, e), 6);
    check(ldexp(1.0, e), 0);
    check(ldexp(1.0, e), 40);
    check(-ldexp(1.0, e) * 3, 3);
  }
  check(from_bits(0x000FFFFFFFFFFFFFULL), 1074);
  check(from_bits(0x0010000000000000ULL), 1074);
  check(from_bits(0x0010000000000000ULL), 20);
}

static void test_signed_zero_infnan(void) {
  expect_str(0.0, 6, "0.000000");
  expect_str(-0.0, 6, "-0.000000");
  expect_str(-0.0, 0, "-0");
  expect_str(0.0, 0, "0");
  expect_str(INFINITY, 6, "inf");
  expect_str(-INFINITY, 6, "-inf");
  expect_str(from_bits(0x7FF8000000000000ULL), 6, "nan");
  expect_str(from_bits(0xFFF8000000000000ULL), 6, "-nan");
  expect_str(from_bits(0x7FF0000000000001ULL), 2, "nan");
  // Negative places means the printf default of six.
  expect_str(1.5, -1, "1.500000");
  expect_str(1.5, -100, "1.500000");
}

static void test_ties_to_even(void) {
  expect_str(0.5, 0, "0");
  expect_str(1.5, 0, "2");
  expect_str(2.5, 0, "2");
  expect_str(3.5, 0, "4");
  expect_str(-2.5, 0, "-2");
  expect_str(0.125, 2, "0.12");
  expect_str(0.375, 2, "0.38");
  expect_str(0.625, 2, "0.62");
  expect_str(0.875, 2, "0.88");
  expect_str(4503599627370496.5, 0, "4503599627370496");
  expect_str(4503599627370497.5, 0, "4503599627370498");
  // Not actually ties in binary, so they round by the true value.
  expect_str(0.045, 2, "0.04");
  expect_str(1.005, 2, "1.00");
  expect_str(2.675, 2, "2.67");
  expect_str(0.15, 1, "0.1");
  expect_str(0.25, 1, "0.2");
  expect_str(0.35, 1, "0.3");
  // Carry all the way through the integer part.
  expect_str(9.9999999, 6, "10.000000");
  expect_str(0.9999999, 6, "1.000000");
  expect_str(999999.9999999, 6, "1000000.000000");
  expect_str(0.99, 0, "1");
  // Decimal literals near a tie are not ties in binary; defer to printf.
  check(9.9999995, 6);
  check(999999.9999995, 6);
  check(0.5e-6, 6);
  check(1.5e-6, 6);
  check(2.5e-6, 6);
}

static void test_buffer_semantics(void) {
  char buf[8];
  memset(buf, 'x', sizeof buf);
  size_t n = ffc_format_double_fixed(buf, 4, 123.456, 3);
  CHECKS++;
  if (n != 7 || memcmp(buf, "123.xxxx", 8) != 0) {
    fprintf(stderr, "FAIL truncation: n=%zu buf=%.8s\n", n, buf);
    FAILS++;
  }
  n = ffc_format_double_fixed(NULL, 0, -DBL_MAX, 10);
  CHECKS++;
  if (n != 1 + 309 + 1 + 10) {
    fprintf(stderr, "FAIL zero-cap length: n=%zu\n", n);
    FAILS++;
  }
  // Exact fit: no byte past cap is touched.
  memset(buf, 'x', sizeof buf);
  n = ffc_format_double_fixed(buf, 7, 123.456, 3);
  CHECKS++;
  if (n != 7 || memcmp(buf, "123.456x", 8) != 0) {
    fprintf(stderr, "FAIL exact fit: n=%zu buf=%.8s\n", n, buf);
    FAILS++;
  }
}

static void test_float_promotion(void) {
  // A float is exactly representable as a double, so formatting the
  // promoted value matches printf("%f", f), which promotes too.
  static const float VALUES[] = {0.1f, 0.3f, 1.0f / 3, 16777217.0f, 3.4028235e38f, 1.17549435e-38f, 1e-45f};
  for (size_t i = 0; i < sizeof VALUES / sizeof *VALUES; i++) {
    check((double)VALUES[i], 6);
    check((double)VALUES[i], 0);
    check((double)VALUES[i], 60);
  }
}

/* -- randomized ------------------------------------------------------- */

static uint64_t rng_state = 0x9E3779B97F4A7C15ULL;
static uint64_t rng_next(void) {
  uint64_t x = rng_state;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  rng_state = x;
  return x;
}

static void test_random_bit_patterns(long iterations) {
  for (long i = 0; i < iterations; i++) {
    uint64_t bits = rng_next();
    double v = from_bits(bits);
    if (isnan(v) || isinf(v)) {
      continue;
    }
    int places = (int)(rng_next() % 25);
    check(v, places);
  }
}

static void test_random_human_scale(long iterations) {
  // Values people actually print: modest exponents, full mantissas.
  for (long i = 0; i < iterations; i++) {
    uint64_t mant = rng_next() >> 11; // 53 bits
    int e = (int)(rng_next() % 120) - 80; // 2^-80 .. 2^39
    double v = ldexp((double)mant, e - 52);
    if (rng_next() & 1) {
      v = -v;
    }
    int places = (int)(rng_next() % 20);
    check(v, places);
    // Occasionally ask for far more digits than the value has.
    if ((i & 63) == 0) {
      check(v, 100 + (int)(rng_next() % 1000));
    }
  }
}

static void test_random_extremes(long iterations) {
  for (long i = 0; i < iterations; i++) {
    uint64_t mant = rng_next() >> 11;
    int e = (int)(rng_next() % 2098) - 1074;
    double v = ldexp((double)mant, e - 52);
    if (isinf(v) || v == 0.0) {
      continue;
    }
    check(v, (int)(rng_next() % 30));
    if ((i & 15) == 0) {
      check(v, 1074);
    }
  }
}

// The two halves of the library must agree: format with enough digits to
// pin the value down, parse it back, and expect the same bits.
static void test_round_trip(long iterations) {
  char buf[BUF_SIZE];
  for (long i = 0; i < iterations; i++) {
    uint64_t mant = rng_next() >> 11;
    int e = (int)(rng_next() % 1000) - 10; // >= 2^-62, so 25 places always carries 17 significant digits
    double v = ldexp((double)mant, e - 52);
    if (isinf(v) || v == 0.0) {
      continue;
    }
    size_t n = ffc_format_double_fixed(buf, sizeof buf, v, 25);
    double back = 0;
    ffc_result r = ffc_parse_double(n, buf, &back);
    CHECKS++;
    if (r.outcome != FFC_OUTCOME_OK || ffc_get_double_bits(back) != ffc_get_double_bits(v)) {
      fprintf(stderr, "FAIL round trip %.17g -> %.*s -> %.17g\n", v, (int)n, buf, back);
      FAILS++;
    }
  }
}

int main(void) {
  test_edge_cases();
  test_signed_zero_infnan();
  test_ties_to_even();
  test_buffer_semantics();
  test_float_promotion();
  test_random_bit_patterns(100000);
  test_random_human_scale(100000);
  test_random_extremes(20000);
  test_round_trip(50000);

  if (FAILS) {
    fprintf(stderr, "\n*** %d of %ld format check(s) FAILED ***\n", FAILS, CHECKS);
    return EXIT_FAILURE;
  }
  printf("format tests: %ld checks passed\n", CHECKS);
  return EXIT_SUCCESS;
}
