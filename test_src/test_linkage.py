#!/usr/bin/env python3
"""Check that the force-inlined API functions stay exported.

Builds the implementation and a caller as separate translation units, in every
C99/C++17 combination, with float inlining both off and on, and links them.
The caller takes the address of each API function so the test fails if the
FFC_IMPL TU inlined the function and failed to emit an external symbol.

Runs with clang/clang++ by default; set CC and CXX to test another toolchain.
"""
import os
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

CALLER = r'''#include "ffc.h"
int main(void) {
  const char input[] = "1.25";
  ffc_result result;

  double d = 0;
  ffc_result (*parse_d)(const char *, const char *, double *) = ffc_from_chars_double;
  result = parse_d(input, input + 4, &d);
  if (result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || d != 1.25) return 1;
  ffc_result (*parse_d_opt)(const char *, const char *, double *, ffc_parse_options) = ffc_from_chars_double_options;
  result = parse_d_opt(input, input + 4, &d, ffc_parse_options_default());
  if (result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || d != 1.25) return 2;

  float f = 0;
  ffc_result (*parse_f)(const char *, const char *, float *) = ffc_from_chars_float;
  result = parse_f(input, input + 4, &f);
  if (result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || f != 1.25f) return 3;
  ffc_result (*parse_f_opt)(const char *, const char *, float *, ffc_parse_options) = ffc_from_chars_float_options;
  result = parse_f_opt(input, input + 4, &f, ffc_parse_options_default());
  if (result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || f != 1.25f) return 4;
  return 0;
}
'''


def run(cmd):
    subprocess.run(cmd, check=True)


def main():
    cc = shlex.split(os.environ.get('CC', 'clang'))
    cxx = shlex.split(os.environ.get('CXX', 'clang++'))
    languages = [('c', 'c99', cc), ('c++', 'c++17', cxx)]
    with tempfile.TemporaryDirectory(prefix='ffc-linkage-') as directory:
        build = Path(directory)
        (build / 'implementation.c').write_text('#define FFC_IMPL\n#include "ffc.h"\n')
        (build / 'caller.c').write_text(CALLER)
        for float_inlining in (False, True):
            flags = ['-DFFC_ENABLE_FLOAT_INLINING'] if float_inlining else []
            for impl, impl_std, impl_compiler in languages:
                run([*impl_compiler, '-x', impl, '-std=' + impl_std, '-O3',
                     '-Wall', *flags, '-I' + str(ROOT), '-c',
                     str(build / 'implementation.c'), '-o', str(build / 'implementation.o')])
                for caller, caller_std, caller_compiler in languages:
                    run([*caller_compiler, '-x', caller, '-std=' + caller_std, '-O3',
                         '-Wall', *flags, '-I' + str(ROOT), '-c',
                         str(build / 'caller.c'), '-o', str(build / 'caller.o')])
                    executable = build / 'linked'
                    run([*cxx, str(build / 'implementation.o'), str(build / 'caller.o'),
                         '-lm', '-o', str(executable)])
                    run([str(executable)])
                    print(f'{" ".join(cc)}: float inlining={float_inlining}: '
                          f'{impl} implementation / {caller} caller PASS', flush=True)


if __name__ == '__main__':
    try:
        main()
    except subprocess.CalledProcessError as error:
        print(f'FAILED: {" ".join(map(str, error.cmd))}', file=sys.stderr)
        sys.exit(1)
