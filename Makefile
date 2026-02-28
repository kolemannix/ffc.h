.PHONY: test example

# Detect linux and define _DEFAULT_SOURCE if so
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    EXTRA_CFLAGS := -D_DEFAULT_SOURCE
endif

CLANG_FLAGS := -Wall -Wextra -Wpedantic -O3 -g -std=c99 $(EXTRA_CFLAGS)

out/test_runner: ffc.h test_src/test.c
	gcc -m64 -Wall -Wextra -Wpedantic ffc.h -fsyntax-only
	gcc -m32 -Wall -Wextra -Wpedantic ffc.h -fsyntax-only
	clang $(CLANG_FLAGS) -I. -Itest_src -DFFC_IMPL test_src/test.c -o out/test_runner -lm

test: out/test_runner out/test_int_runner
	./out/test_runner
	./out/test_int_runner

out/test_int_runner: ffc.h test_src/test_int.c
	clang $(CLANG_FLAGS) -I. -Itest_src -DFFC_IMPL test_src/test_int.c -o out/test_int_runner -lm

ffc.h: src/ffc.h src/common.h src/parse.h src/digit_comparison.h src/api.h src/bigint.h amalgamate.py
	python3 amalgamate.py > ffc.h

out/example: ffc.h example.c
	clang -Wall -Wformat -Wpedantic -std=c99 example.c -o out/example

example: out/example
	./out/example

