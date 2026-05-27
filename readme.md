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

Generated using https://github.com/lemire/simple_fastfloat_benchmark on 2026-03-03

```
$ sudo ./build/benchmarks/benchmark
# parsing random numbers
model: generate random numbers uniformly in the interval [0.0,1.0]
volume: 100000 floats
ASCII volume = 2.09808 MB
loaded db: as3 (Apple silicon)
netlib                                  :   515.59 MB/s (+/- 4.4 %)    24.57 Mfloat/s      28.24 i/B   621.34 i/f (+/- 0.0 %)      7.13 c/B   156.78 c/f (+/- 0.9 %)      3.96 i/c     96.77 b/f    249.72 bm/f      3.85 GHz
strtod                                  :  1102.63 MB/s (+/- 3.2 %)    52.55 Mfloat/s      21.42 i/B   471.13 i/f (+/- 0.0 %)      3.31 c/B    72.82 c/f (+/- 1.3 %)      6.47 i/c     92.00 b/f      0.00 bm/f      3.83 GHz
abseil                                  :  1150.63 MB/s (+/- 1.2 %)    54.84 Mfloat/s      24.01 i/B   528.18 i/f (+/- 0.0 %)      3.13 c/B    68.95 c/f (+/- 0.3 %)      7.66 i/c    115.01 b/f      0.56 bm/f      3.78 GHz
fastfloat                               :  1902.52 MB/s (+/- 1.4 %)    90.68 Mfloat/s      15.19 i/B   334.16 i/f (+/- 0.0 %)      1.90 c/B    41.70 c/f (+/- 0.5 %)      8.01 i/c     57.00 b/f      0.62 bm/f      3.78 GHz
ffc                                     :  2149.68 MB/s (+/- 1.4 %)   102.46 Mfloat/s      13.60 i/B   299.16 i/f (+/- 0.0 %)      1.67 c/B    36.76 c/f (+/- 0.7 %)      8.14 i/c     50.00 b/f      0.62 bm/f      3.77 GHz
$ sudo ./build/benchmarks/benchmark32 # with gcc; all others clang
# parsing random numbers
model: generate random numbers uniformly in the interval [0.0,1.0]
volume: 100000 floats
volume = 2.09808 MB
loaded db: as3 (Apple silicon)
strtof                                  :  1160.10 MB/s (+/- 1.4 %)    55.29 Mfloat/s      20.51 i/B   451.13 i/f (+/- 0.0 %)      3.05 c/B    67.10 c/f (+/- 0.9 %)      6.72 i/c   4897.21 b/f      3.71 GHz
abseil                                  :  1114.52 MB/s (+/- 0.8 %)    53.12 Mfloat/s      24.01 i/B   528.18 i/f (+/- 0.0 %)      3.17 c/B    69.80 c/f (+/- 0.2 %)      7.57 i/c  11501.11 b/f      3.71 GHz
fastfloat                               :  1986.98 MB/s (+/- 3.8 %)    94.70 Mfloat/s      14.64 i/B   322.13 i/f (+/- 0.0 %)      1.82 c/B    40.02 c/f (+/- 0.9 %)      8.05 i/c   5500.09 b/f      3.79 GHz
ffc                                     :  2288.40 MB/s (+/- 7.6 %)   109.07 Mfloat/s      13.23 i/B   291.13 i/f (+/- 0.0 %)      1.63 c/B    35.89 c/f (+/- 0.9 %)      8.11 i/c   5100.09 b/f      3.91 GHz
$ sudo ./build/benchmarks/benchmark -f data/canada.txt
# read 111126 lines
ASCII volume = 1.93374 MB
loaded db: as3 (Apple silicon)
netlib                                  :   490.82 MB/s (+/- 7.2 %)    28.21 Mfloat/s      29.97 i/B   546.94 i/f (+/- 0.0 %)      7.66 c/B   139.68 c/f (+/- 1.4 %)      3.92 i/c     81.77 b/f    189.96 bm/f      3.94 GHz
strtod                                  :   916.32 MB/s (+/- 5.1 %)    52.66 Mfloat/s      24.43 i/B   445.75 i/f (+/- 0.0 %)      4.04 c/B    73.81 c/f (+/- 1.7 %)      6.04 i/c     88.46 b/f      9.55 bm/f      3.89 GHz
abseil                                  :  1038.48 MB/s (+/- 2.6 %)    59.68 Mfloat/s      24.61 i/B   449.03 i/f (+/- 0.0 %)      3.51 c/B    64.12 c/f (+/- 0.2 %)      7.00 i/c     96.53 b/f     10.23 bm/f      3.83 GHz
fastfloat                               :  1302.95 MB/s (+/- 4.9 %)    74.88 Mfloat/s      18.56 i/B   338.62 i/f (+/- 0.0 %)      2.87 c/B    52.45 c/f (+/- 0.4 %)      6.46 i/c     58.90 b/f     10.62 bm/f      3.93 GHz
ffc                                     :  1452.90 MB/s (+/- 5.3 %)    83.49 Mfloat/s      16.73 i/B   305.27 i/f (+/- 0.0 %)      2.57 c/B    46.92 c/f (+/- 1.2 %)      6.51 i/c     54.49 b/f     10.62 bm/f      3.92 GHz
$ sudo ./build/benchmarks/benchmark -f data/mesh.txt
# read 73019 lines
ASCII volume = 0.536009 MB
loaded db: as3 (Apple silicon)
netlib                                  :   657.68 MB/s (+/- 11.8 %)    89.59 Mfloat/s      33.08 i/B   254.62 i/f (+/- 0.0 %)      5.38 c/B    41.43 c/f (+/- 1.6 %)      6.15 i/c     42.56 b/f     41.36 bm/f      3.71 GHz
strtod                                  :   605.46 MB/s (+/- 5.1 %)    82.48 Mfloat/s      33.78 i/B   260.02 i/f (+/- 0.0 %)      5.84 c/B    44.96 c/f (+/- 3.2 %)      5.78 i/c     54.65 b/f     10.57 bm/f      3.71 GHz
abseil                                  :   483.98 MB/s (+/- 4.8 %)    65.93 Mfloat/s      47.48 i/B   365.43 i/f (+/- 0.0 %)      7.57 c/B    58.29 c/f (+/- 2.1 %)      6.27 i/c     80.08 b/f     11.41 bm/f      3.84 GHz
fastfloat                               :   964.77 MB/s (+/- 9.6 %)   131.43 Mfloat/s      26.14 i/B   201.18 i/f (+/- 0.0 %)      4.02 c/B    30.92 c/f (+/- 2.1 %)      6.51 i/c     41.98 b/f      7.73 bm/f      4.06 GHz
ffc                                     :  1019.67 MB/s (+/- 7.3 %)   138.91 Mfloat/s      22.41 i/B   172.53 i/f (+/- 0.0 %)      3.75 c/B    28.89 c/f (+/- 1.5 %)      5.97 i/c     37.98 b/f      7.37 bm/f      4.01 GHz
```

## References

* Daniel Lemire, [Number Parsing at a Gigabyte per
  Second](https://arxiv.org/abs/2101.11408), Software: Practice and Experience
  51 (8), 2021.
* Noble Mushtak, Daniel Lemire, [Fast Number Parsing Without
  Fallback](https://arxiv.org/abs/2212.06644), Software: Practice and Experience
  53 (7), 2023.
