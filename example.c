#include <stdio.h>
#include <string.h>

#define FFC_IMPL
#include "ffc.h"

int main(void) {
   char *input = "-1234.0e10";
   ffc_outcome outcome;
   double d = ffc_parse_double_simple(strlen(input), input, &outcome);
   printf("%s is %f\n", input, d);

   char *int_input = "-42";
   int64_t out;
   ffc_parse_i64(strlen(int_input), int_input, 10, &out);
   printf("%s is %lld\n", int_input, out);

   return 0;
}
