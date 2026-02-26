HEADERS = src/ffc.h src/common.h src/parse.h src/digit_comparison.h src/api.h src/bigint.h

# Detect linux and define _DEFAULT_SOURCE if so
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    EXTRA_CFLAGS := -D_DEFAULT_SOURCE
endif

test_runner: $(HEADERS) test_src/test.c
	clang -Wall -Wformat -O3 -g -std=c11 $(EXTRA_CFLAGS) -Isrc -Itest_src -DFFC_IMPL test_src/test.c -o test_runner -lm

test: test_runner
	./test_runner


