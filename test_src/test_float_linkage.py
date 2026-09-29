#!/usr/bin/env python3
"""Check exported float APIs with separate C99/C++17 implementation and callers."""
import os
import shlex
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CALLER = r'''#include "ffc.h"
int main(void) {
  const char input[] = "1.25";
  float value = 0;
  ffc_result (*parse)(const char *, const char *, float *) = ffc_from_chars_float;
  ffc_result result = parse(input, input + 4, &value);
  if (result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || value != 1.25f) return 1;
  ffc_result (*options)(const char *, const char *, float *, ffc_parse_options) = ffc_from_chars_float_options;
  result = options(input, input + 4, &value, ffc_parse_options_default());
  return result.outcome != FFC_OUTCOME_OK || result.ptr != input + 4 || value != 1.25f;
}
'''


def main():
    cc = shlex.split(os.environ.get('CC', 'clang'))
    cxx = shlex.split(os.environ.get('CXX', 'clang++'))
    languages = [('c', 'c99', cc), ('c++', 'c++17', cxx)]
    with tempfile.TemporaryDirectory(prefix='ffc-float-linkage-') as directory:
        build = Path(directory)
        (build / 'implementation.c').write_text('#define FFC_IMPL\n#include "ffc.h"\n')
        (build / 'caller.c').write_text(CALLER)
        for enabled in [False, True]:
            flags = ['-DFFC_ENABLE_FLOAT_INLINING'] if enabled else []
            for impl, impl_std, impl_compiler in languages:
                subprocess.run([*impl_compiler, '-x', impl, '-std=' + impl_std,
                                '-O3', *flags, '-I' + str(ROOT), '-c',
                                str(build / 'implementation.c'), '-o',
                                str(build / 'implementation.o')], check=True)
                for caller, caller_std, caller_compiler in languages:
                    subprocess.run([*caller_compiler, '-x', caller,
                                    '-std=' + caller_std, '-O3', *flags,
                                    '-I' + str(ROOT), '-c', str(build / 'caller.c'),
                                    '-o', str(build / 'caller.o')], check=True)
                    executable = build / 'linked'
                    subprocess.run([*cxx, str(build / 'implementation.o'),
                                    str(build / 'caller.o'), '-lm', '-o',
                                    str(executable)], check=True)
                    subprocess.run([str(executable)], check=True)
                    print(f'float inlining={enabled}: {impl} implementation / '
                          f'{caller} caller PASS', flush=True)


if __name__ == '__main__':
    main()
