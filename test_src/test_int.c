// Opus 4.6 generated.
// Status: Summarily reviewed and adjusted by knix
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define FFC_DEBUG 0
#include "ffc.h"

static int FAILS = 0;

/* -- helpers ------------------------------------------------------- */

static void verify_i32(const char *input, int32_t expected, int base) {
  size_t len = strlen(input);
  int32_t out = 0;
  ffc_result r = ffc_parse_i32(len, input, base, &out);
  if (r.outcome != FFC_OUTCOME_OK) {
    fprintf(stderr, "FAIL i32 parse \"%s\" base %d: unexpected outcome %d\n",
            input, base, r.outcome);
    FAILS++;
  } else if (out != expected) {
    fprintf(stderr, "FAIL i32 \"%s\" base %d: got %d, expected %d\n",
            input, base, (int)out, (int)expected);
    FAILS++;
  }
}

static void verify_u32(const char *input, uint32_t expected, int base) {
  size_t len = strlen(input);
  uint32_t out = 0;
  ffc_result r = ffc_parse_u32(len, input, base, &out);
  if (r.outcome != FFC_OUTCOME_OK) {
    fprintf(stderr, "FAIL u32 parse \"%s\" base %d: unexpected outcome %d\n",
            input, base, r.outcome);
    FAILS++;
  } else if (out != expected) {
    fprintf(stderr, "FAIL u32 \"%s\" base %d: got %u, expected %u\n",
            input, base, (unsigned)out, (unsigned)expected);
    FAILS++;
  }
}

static void verify_i64(const char *input, int64_t expected, int base) {
  size_t len = strlen(input);
  int64_t out = 0;
  ffc_result r = ffc_parse_i64(len, input, base, &out);
  if (r.outcome != FFC_OUTCOME_OK) {
    fprintf(stderr, "FAIL i64 parse \"%s\" base %d: unexpected outcome %d\n",
            input, base, r.outcome);
    FAILS++;
  } else if (out != expected) {
    fprintf(stderr, "FAIL i64 \"%s\" base %d: got %lld, expected %lld\n",
            input, base, (long long)out, (long long)expected);
    FAILS++;
  }
}

static void verify_u64(const char *input, uint64_t expected, int base) {
  size_t len = strlen(input);
  uint64_t out = 0;
  ffc_result r = ffc_parse_u64(len, input, base, &out);
  if (r.outcome != FFC_OUTCOME_OK) {
    fprintf(stderr, "FAIL u64 parse \"%s\" base %d: unexpected outcome %d\n",
            input, base, r.outcome);
    FAILS++;
  } else if (out != expected) {
    fprintf(stderr, "FAIL u64 \"%s\" base %d: got %llu, expected %llu\n",
            input, base, (unsigned long long)out, (unsigned long long)expected);
    FAILS++;
  }
}

static void expect_outcome_i32(const char *input, int base, ffc_outcome expected_outcome) {
  size_t len = strlen(input);
  int32_t out = 0;
  ffc_result r = ffc_parse_i32(len, input, base, &out);
  if (r.outcome != expected_outcome) {
    fprintf(stderr, "FAIL i32 \"%s\" base %d: got outcome %d, expected %d\n",
            input, base, r.outcome, expected_outcome);
    FAILS++;
  }
}

static void expect_outcome_u32(const char *input, int base, ffc_outcome expected_outcome) {
  size_t len = strlen(input);
  uint32_t out = 0;
  ffc_result r = ffc_parse_u32(len, input, base, &out);
  if (r.outcome != expected_outcome) {
    fprintf(stderr, "FAIL u32 \"%s\" base %d: got outcome %d, expected %d\n",
            input, base, r.outcome, expected_outcome);
    FAILS++;
  }
}

static void expect_outcome_i64(const char *input, int base, ffc_outcome expected_outcome) {
  size_t len = strlen(input);
  int64_t out = 0;
  ffc_result r = ffc_parse_i64(len, input, base, &out);
  if (r.outcome != expected_outcome) {
    fprintf(stderr, "FAIL i64 \"%s\" base %d: got outcome %d, expected %d\n",
            input, base, r.outcome, expected_outcome);
    FAILS++;
  }
}

static void expect_outcome_u64(const char *input, int base, ffc_outcome expected_outcome) {
  size_t len = strlen(input);
  uint64_t out = 0;
  ffc_result r = ffc_parse_u64(len, input, base, &out);
  if (r.outcome != expected_outcome) {
    fprintf(stderr, "FAIL u64 \"%s\" base %d: got outcome %d, expected %d\n",
            input, base, r.outcome, expected_outcome);
    FAILS++;
  }
}

static void verify_ptr_i32(const char *input, int base, const char *expected_tail) {
  size_t len = strlen(input);
  int32_t out = 0;
  ffc_result r = ffc_parse_i32(len, input, base, &out);
  if (strcmp(r.ptr, expected_tail) != 0) {
    fprintf(stderr, "FAIL i32 ptr \"%s\" base %d: tail \"%s\", expected \"%s\"\n",
            input, base, r.ptr, expected_tail);
    FAILS++;
  }
}

/* -- test functions ------------------------------------------------ */

static void test_i32_basic(void) {
  verify_i32("0",              0, 10);
  verify_i32("10 ",           10, 10);
  verify_i32("-40",          -40, 10);
  verify_i32("1001 with text", 1001, 10);
  verify_i32("9.999",            9, 10);
  verify_i32("-1",              -1, 10);
  verify_i32("2147483647",  2147483647, 10);   /* INT32_MAX */
  verify_i32("-2147483648", INT32_MIN,  10);    /* INT32_MIN */
}

static void test_u32_basic(void) {
  verify_u32("0",              0, 10);
  verify_u32("10 ",           10, 10);
  verify_u32("1001 with text", 1001, 10);
  verify_u32("9.999",            9, 10);
  verify_u32("4294967295", 4294967295U, 10);    /* UINT32_MAX */
}

static void test_i64_basic(void) {
  verify_i64("0",              0, 10);
  verify_i64("10 ",           10, 10);
  verify_i64("-40",          -40, 10);
  verify_i64("1001 with text", 1001, 10);
  verify_i64("9.999",            9, 10);
  verify_i64("9223372036854775807",  INT64_MAX, 10);
  verify_i64("-9223372036854775808", INT64_MIN, 10);
}

static void test_u64_basic(void) {
  verify_u64("0",              0, 10);
  verify_u64("10 ",           10, 10);
  verify_u64("1001 with text", 1001, 10);
  verify_u64("9.999",            9, 10);
  verify_u64("18446744073709551615", UINT64_MAX, 10);
}

/* -- invalid argument ---------------------------------------------- */

static void test_i32_invalid(void) {
  const char *cases[] = { "text", "text with 1002", "+50", " 50" };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_i32(cases[i], 10, FFC_OUTCOME_INVALID_INPUT);
}

static void test_u32_invalid(void) {
  const char *cases[] = { "text", "text with 1002", "+50", " 50", "-50" };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_u32(cases[i], 10, FFC_OUTCOME_INVALID_INPUT);
}

static void test_i64_invalid(void) {
  const char *cases[] = { "text", "text with 1002", "+50", " 50" };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_i64(cases[i], 10, FFC_OUTCOME_INVALID_INPUT);
}

static void test_u64_invalid(void) {
  const char *cases[] = { "text", "text with 1002", "+50", " 50", "-50" };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_u64(cases[i], 10, FFC_OUTCOME_INVALID_INPUT);
}

/* -- out of range (decimal) ---------------------------------------- */

static void test_i32_out_of_range(void) {
  const char *cases[] = {
    "2000000000000000000000",
    "2147483648",               /* INT32_MAX + 1 */
    "-2147483649",              /* INT32_MIN - 1 */
    "99999999999",
  };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_i32(cases[i], 10, FFC_OUTCOME_OUT_OF_RANGE);
}

static void test_u32_out_of_range(void) {
  const char *cases[] = {
    "2000000000000000000000",
    "4294967296",               /* UINT32_MAX + 1 */
    "99999999999",
  };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_u32(cases[i], 10, FFC_OUTCOME_OUT_OF_RANGE);
}

static void test_i64_out_of_range(void) {
  const char *cases[] = {
    "2000000000000000000000",
    "9223372036854775808",      /* INT64_MAX + 1 */
    "-9223372036854775809",     /* INT64_MIN - 1 */
  };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_i64(cases[i], 10, FFC_OUTCOME_OUT_OF_RANGE);
}

static void test_u64_out_of_range(void) {
  const char *cases[] = {
    "2000000000000000000000",
    "18446744073709551616",     /* UINT64_MAX + 1 */
  };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++)
    expect_outcome_u64(cases[i], 10, FFC_OUTCOME_OUT_OF_RANGE);
}

/* -- pointer (tail) tests ------------------------------------------ */

static void test_pointer(void) {
  /* numbers only → empty tail */
  verify_ptr_i32("0",   10, "");
  verify_ptr_i32("010", 10, "");
  verify_ptr_i32("-40", 10, "");

  /* string behind numbers */
  verify_ptr_i32("1001 with text",    10, " with text");
  verify_ptr_i32("1001 with text\n",  10, " with text\n");

  /* decimal point */
  verify_ptr_i32("9.999", 10, ".999");

  /* invalid → ptr at start */
  verify_ptr_i32("+50", 10, "+50");

  /* unsigned: string behind */
  {
    const char *input = "1001 with text";
    size_t len = strlen(input);
    uint32_t out = 0;
    ffc_result r = ffc_parse_u32(len, input, 10, &out);
    if (strcmp(r.ptr, " with text") != 0) {
      fprintf(stderr, "FAIL u32 ptr tail: got \"%s\"\n", r.ptr);
      FAILS++;
    }
  }

  /* unsigned: negative is invalid → ptr at start */
  {
    const char *input = "-50";
    size_t len = strlen(input);
    uint32_t out = 0;
    ffc_result r = ffc_parse_u32(len, input, 10, &out);
    if (strcmp(r.ptr, "-50") != 0) {
      fprintf(stderr, "FAIL u32 ptr neg tail: got \"%s\"\n", r.ptr);
      FAILS++;
    }
  }
}

/* -- base 2 -------------------------------------------------------- */

static void test_i32_base2(void) {
  verify_i32("0",   0, 2);
  verify_i32("1",   1, 2);
  verify_i32("100", 4, 2);
  verify_i32("010", 2, 2);
  verify_i32("-1", -1, 2);

  /* invalid digits for base 2 */
  expect_outcome_i32("2", 2, FFC_OUTCOME_INVALID_INPUT);
  expect_outcome_i32("A", 2, FFC_OUTCOME_INVALID_INPUT);
  expect_outcome_i32("-2", 2, FFC_OUTCOME_INVALID_INPUT);
}

static void test_u32_base2(void) {
  verify_u32("0",   0, 2);
  verify_u32("1",   1, 2);
  verify_u32("100", 4, 2);
  verify_u32("010", 2, 2);

  expect_outcome_u32("2",  2, FFC_OUTCOME_INVALID_INPUT);
  expect_outcome_u32("A",  2, FFC_OUTCOME_INVALID_INPUT);
  expect_outcome_u32("-1", 2, FFC_OUTCOME_INVALID_INPUT);
  expect_outcome_u32("-2", 2, FFC_OUTCOME_INVALID_INPUT);
}

/* -- octal --------------------------------------------------------- */

static void test_octal(void) {
  verify_i32("0",    0, 8);
  verify_i32("1",    1, 8);
  verify_i32("07",   7, 8);
  verify_i32("010",  8, 8);
  verify_i32("0011", 9, 8);
}

/* -- hex ----------------------------------------------------------- */

static void test_hex(void) {
  verify_i32("0",     0, 16);
  verify_i32("1",     1, 16);
  verify_i32("F",    15, 16);
  verify_i32("01f",  31, 16);

  /* 0x prefix is not consumed — '0' parses as 0, 'x' stops parsing */
  verify_i32("0x11",  0, 16);

  /* "10X11": '1','0' parse as 16, 'X' stops */
  verify_i32("10X11", 16, 16);
}

/* -- invalid base -------------------------------------------------- */

static void test_invalid_base(void) {
  const char *inputs[] = { "0", "1", "-1", "F", "10Z" };

  for (size_t i = 0; i < sizeof(inputs)/sizeof(*inputs); i++) {
    expect_outcome_i32(inputs[i], -1, FFC_OUTCOME_INVALID_INPUT);
    expect_outcome_i32(inputs[i], 37, FFC_OUTCOME_INVALID_INPUT);
  }
}

/* -- i64 out-of-range per base (from C++ test) -------------------
   Each pair is (positive overflow, negative overflow) for bases 2..36 */

static void test_i64_out_of_range_bases(void) {
  const char *cases[] = {
    /* base  2 */ "1000000000000000000000000000000000000000000000000000000000000000",
                  "-1000000000000000000000000000000000000000000000000000000000000001",
    /* base  3 */ "2021110011022210012102010021220101220222",
                  "-2021110011022210012102010021220101221000",
    /* base  4 */ "20000000000000000000000000000000",
                  "-20000000000000000000000000000001",
    /* base  5 */ "1104332401304422434310311213",
                  "-1104332401304422434310311214",
    /* base  6 */ "1540241003031030222122212",
                  "-1540241003031030222122213",
    /* base  7 */ "22341010611245052052301",
                  "-22341010611245052052302",
    /* base  8 */ "1000000000000000000000",
                  "-1000000000000000000001",
    /* base  9 */ "67404283172107811828",
                  "-67404283172107811830",
    /* base 10 */ "9223372036854775808",
                  "-9223372036854775809",
    /* base 11 */ "1728002635214590698",
                  "-1728002635214590699",
    /* base 12 */ "41A792678515120368",
                  "-41A792678515120369",
    /* base 13 */ "10B269549075433C38",
                  "-10B269549075433C39",
    /* base 14 */ "4340724C6C71DC7A8",
                  "-4340724C6C71DC7A9",
    /* base 15 */ "160E2AD3246366808",
                  "-160E2AD3246366809",
    /* base 16 */ "8000000000000000",
                  "-8000000000000001",
    /* base 17 */ "33D3D8307B214009",
                  "-33D3D8307B21400A",
    /* base 18 */ "16AGH595DF825FA8",
                  "-16AGH595DF825FA9",
    /* base 19 */ "BA643DCI0FFEEHI",
                  "-BA643DCI0FFEEI0",
    /* base 20 */ "5CBFJIA3FH26JA8",
                  "-5CBFJIA3FH26JA9",
    /* base 21 */ "2HEICIIIE82DH98",
                  "-2HEICIIIE82DH99",
    /* base 22 */ "1ADAIBB21DCKFA8",
                  "-1ADAIBB21DCKFA9",
    /* base 23 */ "I6K448CF4192C3",
                  "-I6K448CF4192C4",
    /* base 24 */ "ACD772JNC9L0L8",
                  "-ACD772JNC9L0L9",
    /* base 25 */ "64IE1FOCNN5G78",
                  "-64IE1FOCNN5G79",
    /* base 26 */ "3IGOECJBMCA688",
                  "-3IGOECJBMCA689",
    /* base 27 */ "27C48L5B37OAOQ",
                  "-27C48L5B37OAP0",
    /* base 28 */ "1BK39F3AH3DMQ8",
                  "-1BK39F3AH3DMQ9",
    /* base 29 */ "Q1SE8F0M04ISC",
                  "-Q1SE8F0M04ISD",
    /* base 30 */ "HAJPPBC1FC208",
                  "-HAJPPBC1FC209",
    /* base 31 */ "BM03I95HIA438",
                  "-BM03I95HIA439",
    /* base 32 */ "8000000000000",
                  "-8000000000001",
    /* base 33 */ "5HG4CK9JD4U38",
                  "-5HG4CK9JD4U39",
    /* base 34 */ "3TDTK1V8J6TPQ",
                  "-3TDTK1V8J6TPR",
    /* base 35 */ "2PIJMIKEXRXP8",
                  "-2PIJMIKEXRXP9",
    /* base 36 */ "1Y2P0IJ32E8E8",
                  "-1Y2P0IJ32E8E9",
  };
  size_t n = sizeof(cases) / sizeof(*cases);
  for (size_t i = 0; i < n; i++) {
    int base = 2 + (int)(i / 2);
    expect_outcome_i64(cases[i], base, FFC_OUTCOME_OUT_OF_RANGE);
  }
}

/* -- u64 out-of-range per base ------------------------------------- */

static void test_u64_out_of_range_bases(void) {
  const char *cases[] = {
    /* base  2 */ "10000000000000000000000000000000000000000000000000000000000000000",
    /* base  3 */ "11112220022122120101211020120210210211221",
    /* base  4 */ "100000000000000000000000000000000",
    /* base  5 */ "2214220303114400424121122431",
    /* base  6 */ "3520522010102100444244424",
    /* base  7 */ "45012021522523134134602",
    /* base  8 */ "2000000000000000000000",
    /* base  9 */ "145808576354216723757",
    /* base 10 */ "18446744073709551616",
    /* base 11 */ "335500516A429071285",
    /* base 12 */ "839365134A2A240714",
    /* base 13 */ "219505A9511A867B73",
    /* base 14 */ "8681049ADB03DB172",
    /* base 15 */ "2C1D56B648C6CD111",
    /* base 16 */ "10000000000000000",
    /* base 17 */ "67979G60F5428011",
    /* base 18 */ "2D3FGB0B9CG4BD2G",
    /* base 19 */ "141C8786H1CCAAGH",
    /* base 20 */ "B53BJH07BE4DJ0G",
    /* base 21 */ "5E8G4GGG7G56DIG",
    /* base 22 */ "2L4LF104353J8KG",
    /* base 23 */ "1DDH88H2782I516",
    /* base 24 */ "L12EE5FN0JI1IG",
    /* base 25 */ "C9C336O0MLB7EG",
    /* base 26 */ "7B7N2PCNIOKCGG",
    /* base 27 */ "4EO8HFAM6FLLMP",
    /* base 28 */ "2NC6J26L66RHOG",
    /* base 29 */ "1N3RSH11F098RO",
    /* base 30 */ "14L9LKMO30O40G",
    /* base 31 */ "ND075IB45K86G",
    /* base 32 */ "G000000000000",
    /* base 33 */ "B1W8P7J5Q9R6G",
    /* base 34 */ "7ORP63SH4DPHI",
    /* base 35 */ "5G24A25TWKWFG",
    /* base 36 */ "3W5E11264SGSG",
  };
  size_t n = sizeof(cases) / sizeof(*cases);
  for (size_t i = 0; i < n; i++) {
    int base = 2 + (int)i;
    expect_outcome_u64(cases[i], base, FFC_OUTCOME_OUT_OF_RANGE);
  }
}

/* -- i64 just-within-range per base -------------------------------- */

static void test_i64_within_range_bases(void) {
  const char *cases[] = {
    /* base  2 */ "111111111111111111111111111111111111111111111111111111111111111",
                  "-1000000000000000000000000000000000000000000000000000000000000000",
    /* base  3 */ "2021110011022210012102010021220101220221",
                  "-2021110011022210012102010021220101220222",
    /* base  4 */ "13333333333333333333333333333333",
                  "-20000000000000000000000000000000",
    /* base  5 */ "1104332401304422434310311212",
                  "-1104332401304422434310311213",
    /* base  6 */ "1540241003031030222122211",
                  "-1540241003031030222122212",
    /* base  7 */ "22341010611245052052300",
                  "-22341010611245052052301",
    /* base  8 */ "777777777777777777777",
                  "-1000000000000000000000",
    /* base  9 */ "67404283172107811827",
                  "-67404283172107811828",
    /* base 10 */ "9223372036854775807",
                  "-9223372036854775808",
    /* base 11 */ "1728002635214590697",
                  "-1728002635214590698",
    /* base 12 */ "41A792678515120367",
                  "-41A792678515120368",
    /* base 13 */ "10B269549075433C37",
                  "-10B269549075433C38",
    /* base 14 */ "4340724C6C71DC7A7",
                  "-4340724C6C71DC7A8",
    /* base 15 */ "160E2AD3246366807",
                  "-160E2AD3246366808",
    /* base 16 */ "7FFFFFFFFFFFFFFF",
                  "-8000000000000000",
    /* base 17 */ "33D3D8307B214008",
                  "-33D3D8307B214009",
    /* base 18 */ "16AGH595DF825FA7",
                  "-16AGH595DF825FA8",
    /* base 19 */ "BA643DCI0FFEEHH",
                  "-BA643DCI0FFEEHI",
    /* base 20 */ "5CBFJIA3FH26JA7",
                  "-5CBFJIA3FH26JA8",
    /* base 21 */ "2HEICIIIE82DH97",
                  "-2HEICIIIE82DH98",
    /* base 22 */ "1ADAIBB21DCKFA7",
                  "-1ADAIBB21DCKFA8",
    /* base 23 */ "I6K448CF4192C2",
                  "-I6K448CF4192C3",
    /* base 24 */ "ACD772JNC9L0L7",
                  "-ACD772JNC9L0L8",
    /* base 25 */ "64IE1FOCNN5G77",
                  "-64IE1FOCNN5G78",
    /* base 26 */ "3IGOECJBMCA687",
                  "-3IGOECJBMCA688",
    /* base 27 */ "27C48L5B37OAOP",
                  "-27C48L5B37OAOQ",
    /* base 28 */ "1BK39F3AH3DMQ7",
                  "-1BK39F3AH3DMQ8",
    /* base 29 */ "Q1SE8F0M04ISB",
                  "-Q1SE8F0M04ISC",
    /* base 30 */ "HAJPPBC1FC207",
                  "-HAJPPBC1FC208",
    /* base 31 */ "BM03I95HIA437",
                  "-BM03I95HIA438",
    /* base 32 */ "7VVVVVVVVVVVV",
                  "-8000000000000",
    /* base 33 */ "5HG4CK9JD4U37",
                  "-5HG4CK9JD4U38",
    /* base 34 */ "3TDTK1V8J6TPP",
                  "-3TDTK1V8J6TPQ",
    /* base 35 */ "2PIJMIKEXRXP7",
                  "-2PIJMIKEXRXP8",
    /* base 36 */ "1Y2P0IJ32E8E7",
                  "-1Y2P0IJ32E8E8",
  };
  size_t n = sizeof(cases) / sizeof(*cases);
  for (size_t i = 0; i < n; i++) {
    int base = 2 + (int)(i / 2);
    expect_outcome_i64(cases[i], base, FFC_OUTCOME_OK);
  }
}

/* -- u64 just-within-range per base -------------------------------- */

static void test_u64_within_range_bases(void) {
  const char *cases[] = {
    /* base  2 */ "1111111111111111111111111111111111111111111111111111111111111111",
    /* base  3 */ "11112220022122120101211020120210210211220",
    /* base  4 */ "33333333333333333333333333333333",
    /* base  5 */ "2214220303114400424121122430",
    /* base  6 */ "3520522010102100444244423",
    /* base  7 */ "45012021522523134134601",
    /* base  8 */ "1777777777777777777777",
    /* base  9 */ "145808576354216723756",
    /* base 10 */ "18446744073709551615",
    /* base 11 */ "335500516A429071284",
    /* base 12 */ "839365134A2A240713",
    /* base 13 */ "219505A9511A867B72",
    /* base 14 */ "8681049ADB03DB171",
    /* base 15 */ "2C1D56B648C6CD110",
    /* base 16 */ "FFFFFFFFFFFFFFFF",
    /* base 17 */ "67979G60F5428010",
    /* base 18 */ "2D3FGB0B9CG4BD2F",
    /* base 19 */ "141C8786H1CCAAGG",
    /* base 20 */ "B53BJH07BE4DJ0F",
    /* base 21 */ "5E8G4GGG7G56DIF",
    /* base 22 */ "2L4LF104353J8KF",
    /* base 23 */ "1DDH88H2782I515",
    /* base 24 */ "L12EE5FN0JI1IF",
    /* base 25 */ "C9C336O0MLB7EF",
    /* base 26 */ "7B7N2PCNIOKCGF",
    /* base 27 */ "4EO8HFAM6FLLMO",
    /* base 28 */ "2NC6J26L66RHOF",
    /* base 29 */ "1N3RSH11F098RN",
    /* base 30 */ "14L9LKMO30O40F",
    /* base 31 */ "ND075IB45K86F",
    /* base 32 */ "FVVVVVVVVVVVV",
    /* base 33 */ "B1W8P7J5Q9R6F",
    /* base 34 */ "7ORP63SH4DPHH",
    /* base 35 */ "5G24A25TWKWFF",
    /* base 36 */ "3W5E11264SGSF",
  };
  size_t n = sizeof(cases) / sizeof(*cases);
  for (size_t i = 0; i < n; i++) {
    int base = 2 + (int)i;
    expect_outcome_u64(cases[i], base, FFC_OUTCOME_OK);
  }
}

/* -- i32 out-of-range per base (32-bit boundaries) ---------------
   INT32_MAX =  2147483647
   INT32_MIN = -2147483648
   Selected bases: 2, 8, 10, 16 */

static void test_i32_out_of_range_bases(void) {
  /* base 2:  INT32_MAX = 1111111111111111111111111111111 (31 ones) */
  expect_outcome_i32("10000000000000000000000000000000", 2, FFC_OUTCOME_OUT_OF_RANGE);   /* 2^31     */
  expect_outcome_i32("-10000000000000000000000000000001", 2, FFC_OUTCOME_OUT_OF_RANGE);  /* -(2^31+1) */

  /* base 8:  INT32_MAX = 17777777777 */
  expect_outcome_i32("20000000000", 8, FFC_OUTCOME_OUT_OF_RANGE);
  expect_outcome_i32("-20000000001", 8, FFC_OUTCOME_OUT_OF_RANGE);

  /* base 10 */
  expect_outcome_i32("2147483648", 10, FFC_OUTCOME_OUT_OF_RANGE);
  expect_outcome_i32("-2147483649", 10, FFC_OUTCOME_OUT_OF_RANGE);

  /* base 16: INT32_MAX = 7FFFFFFF */
  expect_outcome_i32("80000000", 16, FFC_OUTCOME_OUT_OF_RANGE);
  expect_outcome_i32("-80000001", 16, FFC_OUTCOME_OUT_OF_RANGE);
}

static void test_i32_within_range_bases(void) {
  /* base 2 */
  expect_outcome_i32("1111111111111111111111111111111", 2, FFC_OUTCOME_OK);     /* INT32_MAX */
  expect_outcome_i32("-10000000000000000000000000000000", 2, FFC_OUTCOME_OK);   /* INT32_MIN */

  /* base 8 */
  expect_outcome_i32("17777777777", 8, FFC_OUTCOME_OK);
  expect_outcome_i32("-20000000000", 8, FFC_OUTCOME_OK);

  /* base 10 */
  expect_outcome_i32("2147483647", 10, FFC_OUTCOME_OK);
  expect_outcome_i32("-2147483648", 10, FFC_OUTCOME_OK);

  /* base 16 */
  expect_outcome_i32("7FFFFFFF", 16, FFC_OUTCOME_OK);
  expect_outcome_i32("-80000000", 16, FFC_OUTCOME_OK);
}

/* -- u32 out-of-range per base ------------------------------------- */

static void test_u32_out_of_range_bases(void) {
  /* base 2: UINT32_MAX = 11111111111111111111111111111111 (32 ones) */
  expect_outcome_u32("100000000000000000000000000000000", 2, FFC_OUTCOME_OUT_OF_RANGE);

  /* base 8: UINT32_MAX = 37777777777 */
  expect_outcome_u32("40000000000", 8, FFC_OUTCOME_OUT_OF_RANGE);

  /* base 10 */
  expect_outcome_u32("4294967296", 10, FFC_OUTCOME_OUT_OF_RANGE);

  /* base 16: UINT32_MAX = FFFFFFFF */
  expect_outcome_u32("100000000", 16, FFC_OUTCOME_OUT_OF_RANGE);
}

static void test_u32_within_range_bases(void) {
  expect_outcome_u32("11111111111111111111111111111111", 2, FFC_OUTCOME_OK);
  expect_outcome_u32("37777777777", 8, FFC_OUTCOME_OK);
  expect_outcome_u32("4294967295", 10, FFC_OUTCOME_OK);
  expect_outcome_u32("FFFFFFFF", 16, FFC_OUTCOME_OK);
}

/* -- leading zeros ------------------------------------------------- */

static void test_leading_zeros(void) {
  /* All of these should parse to 1015 in various bases (from C++ test).
     Testing a subset of bases for i32. */
  const struct { int base; const char *input; } cases[] = {
    {10, "0000000000000000000001015"},
    { 2, "000000000000000000000000000000000000000000000000000000000000000000000011"
         "11110111"},
    { 8, "0000000000000000000000000000001767"},
    {16, "000000000000000000003F7"},
  };
  for (size_t i = 0; i < sizeof(cases)/sizeof(*cases); i++) {
    verify_i32(cases[i].input, 1015, cases[i].base);
  }
}

/* -- issue 235: single-char buffer --------------------------------- */

static void test_issue_235(void) {
  char s = '0';
  int32_t out = -1;
  ffc_result r = ffc_parse_i32(1, &s, 10, &out);
  if (r.outcome != FFC_OUTCOME_OK || out != 0) {
    fprintf(stderr, "FAIL issue 235: outcome=%d out=%d\n", r.outcome, (int)out);
    FAILS++;
  }
}

/* -- main ---------------------------------------------------------- */

int main(void) {
  test_i32_basic();
  test_u32_basic();
  test_i64_basic();
  test_u64_basic();

  test_i32_invalid();
  test_u32_invalid();
  test_i64_invalid();
  test_u64_invalid();

  test_i32_out_of_range();
  test_u32_out_of_range();
  test_i64_out_of_range();
  test_u64_out_of_range();

  test_pointer();

  test_i32_base2();
  test_u32_base2();

  test_octal();
  test_hex();

  test_invalid_base();

  test_i32_out_of_range_bases();
  test_i32_within_range_bases();

  test_u32_out_of_range_bases();
  test_u32_within_range_bases();

  test_i64_out_of_range_bases();
  test_i64_within_range_bases();

  test_u64_out_of_range_bases();
  test_u64_within_range_bases();

  test_leading_zeros();

  test_issue_235();

  if (FAILS) {
    fprintf(stderr, "\n*** %d test(s) FAILED ***\n", FAILS);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
