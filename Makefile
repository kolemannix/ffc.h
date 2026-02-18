test_runner: test_src/test.c src/jkn_ff.h
	clang -O2 -g --std=c11 -Isrc -Itest_src -DJKN_FF_IMPL test_src/test.c -o test_runner

test: test_runner
	./test_runner


