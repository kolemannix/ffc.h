#define FFC_DEBUG 0
#define FFC_IMPL
#include "ffc.h"

#include <errno.h>
#include <inttypes.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define FLOAT_MAXDIGITS_10 9
#define EXHAUSTIVE_TOTAL_WORDS (UINT64_C(1) << 32)

typedef struct exhaustive_task {
  uint32_t worker_index;
  uint32_t worker_count;
  uint32_t sample_stride;
  uint64_t sample_count;
  bool do_long;
  struct progress_state *progress;
} exhaustive_task;

typedef struct progress_state {
  pthread_mutex_t mutex;
  uint64_t processed;
  uint64_t sample_count;
  uint32_t next_percent;
} progress_state;

static uint64_t progress_threshold(uint64_t sample_count, uint32_t percent) {
  return (sample_count * percent + 99) / 100;
}

static void progress_report(progress_state *progress, uint64_t delta) {
  if (delta == 0) {
    return;
  }

  pthread_mutex_lock(&progress->mutex);
  progress->processed += delta;
  while (progress->next_percent <= 100 &&
         progress->processed >= progress_threshold(progress->sample_count, progress->next_percent)) {
    uint64_t milestone = progress_threshold(progress->sample_count, progress->next_percent);
    printf("Progress: %u%% (%" PRIu64 "/%" PRIu64 ")\n",
           progress->next_percent,
           milestone,
           progress->sample_count);
    fflush(stdout);
    progress->next_percent += 10;
  }
  pthread_mutex_unlock(&progress->mutex);
}

static void progress_finish(progress_state *progress) {
  pthread_mutex_lock(&progress->mutex);
  progress->processed = progress->sample_count;
  while (progress->next_percent <= 100) {
    uint64_t milestone = progress_threshold(progress->sample_count, progress->next_percent);
    printf("Progress: %u%% (%" PRIu64 "/%" PRIu64 ")\n",
           progress->next_percent,
           milestone,
           progress->sample_count);
    fflush(stdout);
    progress->next_percent += 10;
  }
  pthread_mutex_unlock(&progress->mutex);
}

static char *float_to_string_long(float f, char *buffer) {
  int written = snprintf(buffer, 128, "%.*e", 64, f);
  return buffer + written;
}

static char *float_to_string(float f, char *buffer) {
  int written = snprintf(buffer, 64, "%.*e", FLOAT_MAXDIGITS_10 - 1, f);
  return buffer + written;
}

static char *double_to_string_long(double d, char *buffer) {
  int written = snprintf(buffer, 128, "%.*e", 64, d);
  return buffer + written;
}

static char *double_to_string(double d, char *buffer) {
  int written = snprintf(buffer, 64, "%.*e", FLOAT_MAXDIGITS_10 - 1, d);
  return buffer + written;
}

static bool double_eq(double exp, double act) {
  if (exp == act) {
    return true;
  }
  if (isnan(exp) && isnan(act)) {
    return signbit(exp) == signbit(act);
  }
  return false;
}

static void test_exhaustive_word(uint64_t w, char *buffer, bool do_long) {
  float f;
  double d;
  uint32_t word32 = (uint32_t)w;

  memcpy(&f, &word32, sizeof(f));
  memcpy(&d, &w, sizeof(d));

  {
    char const *string_end = do_long ? float_to_string_long(f, buffer) : float_to_string(f, buffer);
    float result_value;
    ffc_result result = ffc_from_chars_float(buffer, string_end, &result_value);
    if (result.outcome != FFC_OUTCOME_OK &&
        result.outcome != FFC_OUTCOME_OUT_OF_RANGE) {
      fprintf(stderr, "(32) parsing error ? %s\n", buffer);
      abort();
    }
    if (isnan(f)) {
      if (!isnan(result_value)) {
        fprintf(stderr, "(32) not nan %s\n", buffer);
        abort();
      }
    } else if (copysign(1, result_value) != copysign(1, f)) {
      fprintf(stderr, "(32) %s\n", buffer);
      fprintf(stderr, "(32) I got %a but I was expecting %a\n", result_value, f);
      abort();
    } else if (result_value != f) {
      fprintf(stderr, "(32) fail for w = %" PRIu64 "\n%s got %f expected %f\n", w, buffer, result_value, f);
      fprintf(stderr, "(32) started with %a\n", f);
      fprintf(stderr, "(32) got back     %a\n", result_value);
      abort();
    }
  }

  {
    char const *string_end = do_long ? double_to_string_long(d, buffer) : double_to_string(d, buffer);
    double result_double;
    ffc_result result = ffc_from_chars_double(buffer, string_end, &result_double);
    if (result.outcome != FFC_OUTCOME_OK &&
        result.outcome != FFC_OUTCOME_OUT_OF_RANGE) {
      fprintf(stderr, "(64) parsing error ? %s\n", buffer);
      abort();
    }
    if (!double_eq(result_double, d)) {
      fprintf(stderr, "(64) fail for w = %" PRIu64 "\n%s got %f expected %f\n", w, buffer, result_double, d);
      fprintf(stderr, "(64) started with %a\n", d);
      fprintf(stderr, "(64) got back     %a\n", result_double);
      abort();
    }
  }
}

static void *test_exhaustive_worker(void *arg) {
  exhaustive_task *task = (exhaustive_task *)arg;
  char buffer[128];
  uint64_t pending_progress = 0;
  for (uint64_t sample_index = task->worker_index; sample_index < task->sample_count;
       sample_index += task->worker_count) {
    uint64_t w = sample_index * task->sample_stride;
    test_exhaustive_word(w, buffer, task->do_long);
    pending_progress++;
    if (pending_progress >= 4096) {
      progress_report(task->progress, pending_progress);
      pending_progress = 0;
    }
  }
  progress_report(task->progress, pending_progress);
  return NULL;
}

static uint32_t parse_u32_or_die(const char *value, const char *flag) {
  errno = 0;
  char *end = NULL;
  unsigned long parsed = strtoul(value, &end, 10);
  if (errno != 0 || end == value || *end != '\0' || parsed > UINT32_MAX) {
    fprintf(stderr, "invalid value for %s: %s\n", flag, value);
    exit(2);
  }
  return (uint32_t)parsed;
}

static int exhaustive_32_run(uint32_t thread_count, uint32_t sample_stride, bool do_long) {
  if (sample_stride == 0) {
    fprintf(stderr, "--sample-stride must be greater than 0\n");
    return 2;
  }

  uint64_t sample_count = (EXHAUSTIVE_TOTAL_WORDS + sample_stride - 1) / sample_stride;
  if (sample_count == 0) {
    fprintf(stderr, "no exhaustive workload selected\n");
    return 2;
  }

  if (thread_count == 0) {
    thread_count = 1;
  }
  if ((uint64_t)thread_count > sample_count) {
    thread_count = (uint32_t)sample_count;
  }

  printf("Running exhaustive test: threads=%u sample_stride=%u (~%.2f%% of %" PRIu64 " values)\n",
         thread_count,
         sample_stride,
         100.0 / (double)sample_stride,
         EXHAUSTIVE_TOTAL_WORDS);

  pthread_t *threads = calloc(thread_count, sizeof(*threads));
  exhaustive_task *tasks = calloc(thread_count, sizeof(*tasks));
  if (threads == NULL || tasks == NULL) {
    fprintf(stderr, "failed to allocate thread structures\n");
    free(threads);
    free(tasks);
    return 1;
  }

  progress_state progress = {0};
  progress.processed = 0;
  progress.sample_count = sample_count;
  progress.next_percent = 10;
  if (pthread_mutex_init(&progress.mutex, NULL) != 0) {
    fprintf(stderr, "failed to initialize progress mutex\n");
    free(threads);
    free(tasks);
    return 1;
  }

  struct timespec begin = {0};
  struct timespec end = {0};
  clock_gettime(CLOCK_MONOTONIC, &begin);

  uint32_t created_threads = 0;
  for (uint32_t i = 0; i < thread_count; i++) {
    tasks[i].worker_index = i;
    tasks[i].worker_count = thread_count;
    tasks[i].sample_stride = sample_stride;
    tasks[i].sample_count = sample_count;
    tasks[i].do_long = do_long;
    tasks[i].progress = &progress;

    int err = pthread_create(&threads[i], NULL, test_exhaustive_worker, &tasks[i]);
    if (err != 0) {
      fprintf(stderr, "pthread_create failed: %s\n", strerror(err));
      for (uint32_t j = 0; j < created_threads; j++) {
        pthread_join(threads[j], NULL);
      }
      pthread_mutex_destroy(&progress.mutex);
      free(threads);
      free(tasks);
      return 1;
    }
    created_threads++;
  }

  for (uint32_t i = 0; i < thread_count; i++) {
    int err = pthread_join(threads[i], NULL);
    if (err != 0) {
      fprintf(stderr, "pthread_join failed: %s\n", strerror(err));
      pthread_mutex_destroy(&progress.mutex);
      free(threads);
      free(tasks);
      return 1;
    }
  }

  progress_finish(&progress);
  clock_gettime(CLOCK_MONOTONIC, &end);
  pthread_mutex_destroy(&progress.mutex);
  free(threads);
  free(tasks);

  double elapsed_s =
      (double)(end.tv_sec - begin.tv_sec) + (double)(end.tv_nsec - begin.tv_nsec) / 1000000000.0;
  printf("Exhaustive run completed: checked %" PRIu64 " values in %.2f seconds\n", sample_count, elapsed_s);
  puts("all ok");
  return 0;
}

int main(void) {
  uint32_t thread_count = 1;
  uint32_t sample_stride = 1;

  const char *threads_env = getenv("FFC_EXH_THREADS");
  if (threads_env == NULL || threads_env[0] == '\0') {
    fprintf(stderr, "warning: FFC_EXH_THREADS is not set; using 1 thread\n");
  } else {
    thread_count = parse_u32_or_die(threads_env, "FFC_EXH_THREADS");
  }

  const char *sample_stride_env = getenv("FFC_EXH_SAMPLE_STRIDE");
  if (sample_stride_env != NULL && sample_stride_env[0] != '\0') {
    sample_stride = parse_u32_or_die(sample_stride_env, "FFC_EXH_SAMPLE_STRIDE");
  }

  return exhaustive_32_run(thread_count, sample_stride, true);
}
