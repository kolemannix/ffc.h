.PHONY: test example fetch-supplemental-data

# Detect linux and define _DEFAULT_SOURCE if so
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    EXTRA_CFLAGS := -D_DEFAULT_SOURCE
endif

CLANG_FLAGS := -xc -Wall -Wextra -Wpedantic -O3 -g -std=c99 $(EXTRA_CFLAGS)
SUPPLEMENTAL_TEST_FILES_DIR := out/supplemental_test_files
SUPPLEMENTAL_TEST_DATA_DIR := $(SUPPLEMENTAL_TEST_FILES_DIR)/data
SUPPLEMENTAL_TEST_FILES_REPO := https://github.com/fastfloat/supplemental_test_files.git

out/test_runner: ffc.h test_src/test.c | out
	gcc -xc -Wall -Wextra -Wpedantic ffc.h -fsyntax-only
	clang $(CLANG_FLAGS) -I. -Itest_src test_src/test.c -o out/test_runner -lm

test: out/test_runner out/test_int_runner out/supplemental_tests out/supplemental_tests_src
	./out/test_runner
	./out/test_int_runner
	./out/supplemental_tests
	./out/supplemental_tests_src

out/test_int_runner: ffc.h test_src/test_int.c | out
	clang $(CLANG_FLAGS) -I. -Itest_src test_src/test_int.c -o out/test_int_runner -lm

$(SUPPLEMENTAL_TEST_DATA_DIR):
	@test -d "$(SUPPLEMENTAL_TEST_DATA_DIR)" || { \
		echo "Missing supplemental test data at $(SUPPLEMENTAL_TEST_DATA_DIR)." >&2; \
		echo "Run 'make fetch-supplemental-data' first." >&2; \
		exit 1; \
	}

out/supplemental_tests: ffc.h test_src/supplemental_tests.c $(SUPPLEMENTAL_TEST_DATA_DIR) | out
	clang $(CLANG_FLAGS) -I. -Itest_src -DSUPPLEMENTAL_TEST_DATA_DIR=\"$(SUPPLEMENTAL_TEST_DATA_DIR)\" test_src/supplemental_tests.c -o out/supplemental_tests -lm

out/supplemental_tests_src: src/ffc.h test_src/supplemental_tests.c $(SUPPLEMENTAL_TEST_DATA_DIR) | out
	clang $(CLANG_FLAGS) -Isrc -Itest_src -DSUPPLEMENTAL_TEST_DATA_DIR=\"$(SUPPLEMENTAL_TEST_DATA_DIR)\" test_src/supplemental_tests.c -o out/supplemental_tests_src -lm

fetch-supplemental-data: | out
	git clone --depth 1 $(SUPPLEMENTAL_TEST_FILES_REPO) $(SUPPLEMENTAL_TEST_FILES_DIR)

ffc.h: src/ffc.h src/common.h src/parse.h src/digit_comparison.h src/api.h src/bigint.h amalgamate.py
	python3 amalgamate.py > ffc.h

out/example: ffc.h example.c | out
	clang -xc -Wall -Wformat -Wpedantic -std=c99 example.c -o out/example

example: out/example
	./out/example

out:
	mkdir -p out
