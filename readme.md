## ffc.h
A direct and faithful c99 single-header port of Daniel Lemire's fast_float library.

See [fast_float](https://github.com/fastfloat/fast_float) for much more information on the algorithm and other
characteristics of the approach.

Example
```c
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
```

For use within a larger parser, where you don't expect to reach the end of input, use
the non-simple variants as the `ffc_result` includes the stopping point, just like in fast_float

## API

### Float Parsing

- `double ffc_parse_double_simple(size_t len, const char *s, ffc_outcome *outcome)`  
  Parses a double from a string of given length. Returns the parsed value, outcome indicates success/failure.

- `ffc_result ffc_parse_double(size_t len, const char *s, double *out)`  
  Parses a double from a string, storing result in `out`. Returns `ffc_result` with outcome and end pointer.

- `float ffc_parse_float_simple(size_t len, const char *s, ffc_outcome *outcome)`  
  Parses a float from a string of given length. Returns the parsed value.

- `ffc_result ffc_parse_float(size_t len, const char *s, float *out)`  
  Parses a float from a string, storing result in `out`.

### Integer Parsing

- `ffc_result ffc_parse_i64(size_t len, const char *s, int base, int64_t *out)`  
  Parses a signed 64-bit integer from string with given base.

- `ffc_result ffc_parse_u64(size_t len, const char *s, int base, uint64_t *out)`  
  Parses an unsigned 64-bit integer from string with given base.

### Types

- `ffc_outcome`: Enum indicating parse result (OK, OUT_OF_RANGE, INVALID_INPUT)
- `ffc_result`: Struct with `ptr` (end of parsed string) and `outcome`

## Building

### With Make

Use the provided Makefile:

```bash
make test
make example
```

`make test` builds and runs the base tests plus supplemental tests.
Run `make fetch-supplemental-data` once first to clone `supplemental_test_files`
into `out/`.

### With CMake

ffc.h supports building with CMake as an installable single-header library.

```bash
cmake -B build
cmake --build build
ctest --test-dir build
cmake --install build # Install the library
```

The CMake build creates test executables for both the amalgamated header and the separate src/ headers.


To use ffc.h as a dependency in your CMake project, you have two options:

#### Using FetchContent

```cmake
include(FetchContent)
FetchContent_Declare(
    ffc
    GIT_REPOSITORY https://github.com/kolemannix/ffc.h.git
    GIT_TAG main
)
FetchContent_MakeAvailable(ffc)
target_link_libraries(your_target ffc::ffc)
```

#### Using CPM.cmake

```cmake
include(cpm)
CPMAddPackage("gh:dlemire/ffc.h#main")
target_link_libraries(your_target ffc::ffc)
```

## Benchmarks

Generated using https://github.com/lemire/simple_fastfloat_benchmark on 2026-05-26.
5-run averages on dedicated bare-metal servers (no cloud noise).

### x86 — Intel Xeon Platinum 8488C (AWS m7i.metal-24xl), GCC, `-O3`

| Dataset | ffc MB/s | fastfloat MB/s | Δ |
|---------|----------|----------------|---|
| random [0,1] | **1960** | 2018 | −3% |
| canada.txt | **1565** | 1407 | **+11% (ffc leads)** |
| mesh.txt | **1187** | 1124 | **+6% (ffc leads)** |

### ARM — Graviton4 (AWS m8g.metal-24xl), GCC, `-O3`

| Dataset | ffc MB/s | fastfloat MB/s | Δ |
|---------|----------|----------------|---|
| random [0,1] | **1795** | 1085 | **+65% (ffc leads)** |
| canada.txt | **1512** | 891 | **+70% (ffc leads)** |
| mesh.txt | **1257** | 498 | **+152% (ffc leads)** |

ffc leads or matches fastfloat on every dataset on both architectures.

## References

* Daniel Lemire, [Number Parsing at a Gigabyte per
  Second](https://arxiv.org/abs/2101.11408), Software: Practice and Experience
  51 (8), 2021.
* Noble Mushtak, Daniel Lemire, [Fast Number Parsing Without
  Fallback](https://arxiv.org/abs/2212.06644), Software: Practice and Experience
  53 (7), 2023.
