## ffc.h
A direct and faithful c99 single-header port of Daniel Lemire's fast_float library.

See [fast_float](https://github.com/fastfloat/fast_float) for much more information on the algorithm and other
characteristics of the approach.

Example
```c
#include <stdio.h>
#include <string.h>

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

## Building

### With Make

Use the provided Makefile:

```bash
make test
make example
```

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
    GIT_REPOSITORY https://github.com/dlemire/ffc.h.git
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


## Caveats
- I have not benchmarked yet; we need to confirm that constant folding, and thus branch elimination, is occurring
as intended for float/double paths and the 4 integer paths.
- Does not support wide chars; only 1-byte strings (e.g., UTF8) are supported.
- The 32-bit architecture code is untested
