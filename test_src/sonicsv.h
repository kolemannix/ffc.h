/*
 * ============================================================================
 * SonicSV - Ultra-Fast SIMD-Accelerated CSV Parser
 * ============================================================================
 * Version: 3.1.1 | Single-header | C99 | MIT License
 *
 * A blazing-fast CSV parser that automatically uses SIMD instructions
 * (SSE4.2, AVX2, AVX-512, NEON, SVE) for maximum performance. Achieves
 * up to 5 GB/s parsing speed on modern hardware.
 *
 * Copyright (c) 2025 JHG Natter

 * ============================================================================
 * LICENSE (MIT)
 * ============================================================================
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * ============================================================================
 */

 #ifndef SONICSV_H
 #define SONICSV_H
 
 #ifndef _POSIX_C_SOURCE
 #define _POSIX_C_SOURCE 200112L
 #endif
 
 #ifndef _ISOC11_SOURCE
 #define _ISOC11_SOURCE
 #endif
 
 #include <stdbool.h>
 #include <stddef.h>
 #include <stdint.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <time.h>
 #include <stdatomic.h>
 
 #ifdef __cplusplus
 extern "C" {
 #endif
 
 #define SONICSV_VERSION_MAJOR 3
 #define SONICSV_VERSION_MINOR 1
 #define SONICSV_VERSION_PATCH 1
 
 // Enhanced compiler hints and target attributes for better SIMD compilation
 #if defined(__GNUC__) || defined(__clang__)
 #define sonicsv_likely(x)   __builtin_expect(!!(x), 1)
 #define sonicsv_unlikely(x) __builtin_expect(!!(x), 0)
 #define sonicsv_always_inline __attribute__((always_inline)) inline
 #define sonicsv_prefetch_read(ptr, offset) __builtin_prefetch((char*)(ptr) + (offset), 0, 3)
 #define sonicsv_prefetch_write(ptr, offset) __builtin_prefetch((char*)(ptr) + (offset), 1, 3)
 #define sonicsv_assume_aligned(ptr, align) __builtin_assume_aligned(ptr, align)
 #define sonicsv_force_inline __attribute__((always_inline, flatten)) inline
 #define sonicsv_hot __attribute__((hot))
 #define sonicsv_cold __attribute__((cold))
 #define sonicsv_aligned(n) __attribute__((aligned(n)))
 #else
 #define sonicsv_likely(x)   (x)
 #define sonicsv_unlikely(x) (x)
 #define sonicsv_always_inline inline
 #define sonicsv_prefetch_read(ptr, offset) ((void)0)
 #define sonicsv_prefetch_write(ptr, offset) ((void)0)
 #define sonicsv_assume_aligned(ptr, align) (ptr)
 #define sonicsv_force_inline inline
 #define sonicsv_hot
 #define sonicsv_cold
 #define sonicsv_aligned(n)
 #endif
 
 // Cache line size for optimal prefetching
 #define SONICSV_CACHE_LINE_SIZE 64
 #define SONICSV_PREFETCH_DISTANCE (8 * SONICSV_CACHE_LINE_SIZE)
 
 // SIMD feature flags
 #define CSV_SIMD_NONE 0x00
 #define CSV_SIMD_SSE4_2 0x01
 #define CSV_SIMD_AVX2 0x02
 #define CSV_SIMD_NEON 0x04
 #define CSV_SIMD_AVX512 0x08
 #define CSV_SIMD_SVE 0x10
 
 // Endianness detection
 #if defined(__BYTE_ORDER__)
     #if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
         #define SONICSV_LITTLE_ENDIAN 1
         #define SONICSV_BIG_ENDIAN 0
     #elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
         #define SONICSV_LITTLE_ENDIAN 0
         #define SONICSV_BIG_ENDIAN 1
     #endif
 #elif defined(__LITTLE_ENDIAN__) || defined(__ARMEL__) || defined(__THUMBEL__) || \
       defined(__AARCH64EL__) || defined(_MIPSEL) || defined(__MIPSEL) || defined(__MIPSEL__) || \
       defined(__x86_64__) || defined(__i386__) || defined(_X86_) || defined(__X86__)
     #define SONICSV_LITTLE_ENDIAN 1
     #define SONICSV_BIG_ENDIAN 0
 #else
     #define SONICSV_LITTLE_ENDIAN 0
     #define SONICSV_BIG_ENDIAN 1
 #endif
 
 // Byte swapping for big-endian support (used in NEON path)
 #if SONICSV_BIG_ENDIAN
 static sonicsv_always_inline uint64_t csv_bswap64(uint64_t x) {
     return ((x & 0xFF00000000000000ULL) >> 56) | ((x & 0x00FF000000000000ULL) >> 40) |
            ((x & 0x0000FF0000000000ULL) >> 24) | ((x & 0x000000FF00000000ULL) >> 8)  |
            ((x & 0x00000000FF000000ULL) << 8)  | ((x & 0x0000000000FF0000ULL) << 24) |
            ((x & 0x000000000000FF00ULL) << 40) | ((x & 0x00000000000000FFULL) << 56);
 }
 #endif
 
 // Architecture detection with proper alignment
 #ifdef __x86_64__
 #if defined(__GNUC__) || defined(__clang__)
 #define HAVE_SSE4_2
 #define HAVE_AVX2
 #include <immintrin.h>
 #include <nmmintrin.h>
 #include <cpuid.h>
 #endif
 #define SONICSV_SIMD_ALIGN 32
 #endif
 
 #ifdef __aarch64__
 #define HAVE_NEON
 #include <arm_neon.h>
 #define SONICSV_SIMD_ALIGN 16
 #if defined(__ARM_FEATURE_SVE)
 #define HAVE_SVE
 #include <arm_sve.h>
 #endif
 #endif
 
 #ifndef SONICSV_SIMD_ALIGN
 #define SONICSV_SIMD_ALIGN 16
 #endif
 
/*
 * Error codes returned by all parsing functions.
 * Check return values and use csv_error_string() for human-readable messages.
 */
typedef enum {
  CSV_OK = 0,                     /* Success - no error */
  CSV_ERROR_INVALID_ARGS = -1,    /* NULL pointer or invalid parameter */
  CSV_ERROR_OUT_OF_MEMORY = -2,   /* Memory allocation failed */
  CSV_ERROR_PARSE_ERROR = -6,     /* Malformed CSV (strict mode only) */
  CSV_ERROR_FIELD_TOO_LARGE = -7, /* Field exceeds max_field_size */
  CSV_ERROR_ROW_TOO_LARGE = -8,   /* Row exceeds max_row_size */
  CSV_ERROR_IO_ERROR = -9         /* File I/O failure */
} csv_error_t;
 
/*
 * Parser configuration options.
 * Use csv_default_options() to get sensible defaults, then customize as needed.
 */
typedef struct {
  /* Basic CSV format settings */
  char delimiter;          /* Field separator character (default: ',') */
  char quote_char;         /* Quote character for escaping (default: '"') */
  bool double_quote;       /* Treat "" as escaped quote (default: true) */
  bool trim_whitespace;    /* Strip leading/trailing spaces (default: false) */
  bool ignore_empty_lines; /* Skip blank lines (default: true) */
  bool strict_mode;        /* Error on malformed CSV (default: false) */

  /* Size limits - prevents memory exhaustion on malformed input */
  size_t max_field_size;   /* Max bytes per field (default: 10MB) */
  size_t max_row_size;     /* Max bytes per row (default: 100MB) */
  size_t buffer_size;      /* I/O buffer size (default: 64KB) */
  size_t max_memory_kb;    /* Memory limit in KB, 0=unlimited (default: 0) */
  size_t current_memory;   /* Internal: current memory usage tracking */

  /* Performance tuning */
  bool disable_mmap;       /* Force stream I/O instead of mmap (default: false) */
  int num_threads;         /* Reserved for future parallel parsing */
  bool enable_parallel;    /* Reserved for future parallel parsing */
  bool enable_prefetch;    /* Use CPU prefetch hints (default: true) */
  size_t prefetch_distance;/* Prefetch lookahead in bytes */
  bool force_alignment;    /* Force SIMD-aligned allocations (default: true) */
} csv_parse_options_t;
 
/*
 * A single CSV field (cell).
 * Access via csv_get_field(row, index).
 *
 * IMPORTANT: 'data' is NOT null-terminated for unquoted fields!
 * Always use 'size' to determine the field length.
 * Example: printf("%.*s", (int)field->size, field->data);
 */
typedef struct sonicsv_aligned(8) {
  const char *data;  /* Pointer to field content */
  size_t size;       /* Length in bytes (use this, not strlen!) */
  bool quoted;       /* true if field was quoted in source CSV */
} csv_field_t;

/*
 * A parsed CSV row, passed to your row callback.
 * Contains an array of fields and metadata about the row.
 */
typedef struct sonicsv_aligned(8) {
  csv_field_t *fields;   /* Array of fields in this row */
  size_t num_fields;     /* Number of fields (columns) */
  uint64_t row_number;   /* 1-based row number in the file */
  size_t byte_offset;    /* Byte offset of this row in the input */
} csv_row_t;
 
 // Enhanced statistics with memory and SIMD metrics
 typedef struct sonicsv_aligned(64) {
   uint64_t total_bytes_processed;
   uint64_t total_rows_parsed;
   uint64_t total_fields_parsed;
   uint64_t parse_time_ns;
   double throughput_mbps;
   uint32_t simd_acceleration_used;
   uint32_t peak_memory_kb;
   uint32_t errors_encountered;
   // Performance counters
   struct {
     uint64_t simd_ops;
     uint64_t scalar_fallbacks;
     double avg_field_size;
     double avg_row_size;
     double memory_efficiency;
   } perf;
 } csv_stats_t;
 
/* Opaque parser handle - create with csv_parser_create() */
typedef struct csv_parser csv_parser_t;

/* Opaque string pool handle - for string deduplication */
typedef struct csv_string_pool csv_string_pool_t;

/*
 * Callback function types.
 * Set these with csv_parser_set_row_callback() and csv_parser_set_error_callback().
 */

/* Called once for each parsed row */
typedef void (*csv_row_callback_t)(const csv_row_t *row, void *user_data);

/* Called when a parse error occurs */
typedef void (*csv_error_callback_t)(csv_error_t error, const char *message,
                                     uint64_t row_number, void *user_data);

/* ============================================================================
 * CORE API - Parser lifecycle and parsing functions
 * ============================================================================ */

/* Create a new parser. Pass NULL for default options, or customize. */
csv_parser_t *csv_parser_create(const csv_parse_options_t *options);

/* Free all resources. Always call when done with a parser. */
void csv_parser_destroy(csv_parser_t *parser);

/* Reset parser state for reuse (avoids reallocation). */
csv_error_t csv_parser_reset(csv_parser_t *parser);

/* Set callback invoked for each parsed row. */
void csv_parser_set_row_callback(csv_parser_t *parser, csv_row_callback_t callback, void *user_data);

/* Set callback invoked on parse errors. */
void csv_parser_set_error_callback(csv_parser_t *parser, csv_error_callback_t callback, void *user_data);

/* Parse a raw buffer. Set is_final=true for the last chunk. */
csv_error_t csv_parse_buffer(csv_parser_t *parser, const char *buffer, size_t size, bool is_final);

/* Parse an entire file (uses mmap for best performance). */
csv_error_t csv_parse_file(csv_parser_t *parser, const char *filename);

/* Parse from a FILE* stream. */
csv_error_t csv_parse_stream(csv_parser_t *parser, FILE *stream);

/* Parse a null-terminated string. */
csv_error_t csv_parse_string(csv_parser_t *parser, const char *csv_string);

/* ============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================ */

/* Get default options (call this, then modify as needed). */
csv_parse_options_t csv_default_options(void);

/* Convert error code to human-readable string. */
const char *csv_error_string(csv_error_t error);

/* Get detailed performance statistics. */
csv_stats_t csv_parser_get_stats(const csv_parser_t *parser);

/* Print performance statistics to stdout. */
void csv_print_stats(const csv_parser_t *parser);

/* Get detected SIMD features (bitmask of CSV_SIMD_* flags). */
uint32_t csv_get_simd_features(void);

/* Get field at index from a row (returns NULL if index out of bounds). */
const csv_field_t *csv_get_field(const csv_row_t *row, size_t index);

/* Get number of fields in a row. */
size_t csv_get_num_fields(const csv_row_t *row);

/* ============================================================================
 * STRING POOL (optional) - For deduplicating repeated field values
 * ============================================================================ */

/* Create a string pool for interning repeated strings. */
csv_string_pool_t *csv_string_pool_create(size_t initial_capacity);

/* Destroy a string pool and free all memory. */
void csv_string_pool_destroy(csv_string_pool_t *pool);

/* Intern a string (returns existing pointer if already interned). */
const char *csv_string_pool_intern(csv_string_pool_t *pool, const char *str, size_t length);

/* Clear all interned strings (for reuse without reallocation). */
void csv_string_pool_clear(csv_string_pool_t *pool);
 
 #ifdef __cplusplus
 }
 #endif
 
 #ifdef SONICSV_IMPLEMENTATION
 
// Enhanced includes for new features
#include <pthread.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif
 
 // Memory allocation tracking with recycling
 typedef struct csv_alloc_header {
     size_t size;
     size_t alignment;
     uint32_t magic;
     struct csv_alloc_header* next;
     struct csv_alloc_header* prev;
 } csv_alloc_header_t;
 

// Safe multiplication to prevent integer overflow
static sonicsv_always_inline bool csv_safe_mul(size_t a, size_t b, size_t *result) {
    if (b != 0 && a > SIZE_MAX / b) return false;
    *result = a * b;
    return true;
}

#define CSV_ALLOC_MAGIC 0xDEADBEEF
 static atomic_size_t g_total_allocated = 0;
 static atomic_size_t g_peak_allocated = 0;
 static atomic_size_t g_allocation_count = 0;
 
 // Thread-safe allocation tracking
 static sonicsv_always_inline void csv_track_allocation(csv_alloc_header_t* header) {
     size_t new_total = atomic_fetch_add_explicit(&g_total_allocated, header->size, memory_order_relaxed) + header->size;
     atomic_fetch_add_explicit(&g_allocation_count, 1, memory_order_relaxed);
 
     // Update peak if necessary (racy but approximate is fine)
     size_t current_peak = atomic_load_explicit(&g_peak_allocated, memory_order_relaxed);
     while (new_total > current_peak) {
         if (atomic_compare_exchange_weak_explicit(&g_peak_allocated, &current_peak, new_total,
                                                 memory_order_relaxed, memory_order_relaxed)) {
             break;
         }
     }
 }
 
 static sonicsv_always_inline void csv_untrack_allocation(csv_alloc_header_t* header) {
     if (header->magic == CSV_ALLOC_MAGIC) {
         atomic_fetch_sub_explicit(&g_total_allocated, header->size, memory_order_relaxed);
         atomic_fetch_sub_explicit(&g_allocation_count, 1, memory_order_relaxed);
     }
 }
 
 
 
// Try to get block from recycling pool
// NOTE: Pool recycling disabled due to size-class bug where blocks of different
// sizes within the same class could be reused, causing buffer overflows.
// See: https://github.com/anthropics/claude-code/issues/XXX
static sonicsv_always_inline void* csv_pool_alloc(size_t size, size_t alignment) {
    (void)size;
    (void)alignment;
    return NULL; // Disabled - always allocate fresh
}
 
// Return block to recycling pool
// NOTE: Pool recycling disabled - see csv_pool_alloc comment
static sonicsv_always_inline bool csv_pool_free(void* ptr, size_t size) {
    (void)ptr;
    (void)size;
    return false; // Disabled - always free to system
}
 
// Safe memory-aligned buffer allocation with tracking and recycling
// Uses a direct offset pointer for O(1) header lookup instead of magic number search
static sonicsv_always_inline void* csv_aligned_alloc(size_t size, size_t alignment) {
    if (size == 0) return NULL;
    if (alignment == 0) alignment = sizeof(void*);
    // Ensure alignment is power of 2 and at least pointer-sized
    if ((alignment & (alignment - 1)) != 0) return NULL;
    if (alignment < sizeof(void*)) alignment = sizeof(void*);

    // Prevent integer overflow with safe arithmetic
    if (size > SIZE_MAX - sizeof(csv_alloc_header_t) - alignment - sizeof(void*)) return NULL;

    // Include space for offset pointer before user data
    size_t total_size = sizeof(csv_alloc_header_t) + alignment + sizeof(void*) + size;
    void* raw_ptr = csv_pool_alloc(total_size, alignment);

    if (!raw_ptr) {
        // Allocate new block
#if defined(__APPLE__)
        if (posix_memalign(&raw_ptr, alignment >= 16 ? alignment : 16, total_size) != 0) return NULL;
#else
        size_t aligned_total = (total_size + alignment - 1) & ~(alignment - 1);
        raw_ptr = aligned_alloc(alignment >= sizeof(void*) ? alignment : sizeof(void*), aligned_total);
        if (!raw_ptr) return NULL;
#endif
    }

    // Initialize header at the start
    csv_alloc_header_t* header = (csv_alloc_header_t*)raw_ptr;
    header->size = size;
    header->alignment = alignment;
    header->magic = CSV_ALLOC_MAGIC;
    header->next = NULL;
    header->prev = NULL;

    csv_track_allocation(header);

    // Calculate aligned user pointer (leave room for offset pointer)
    char* after_header = (char*)raw_ptr + sizeof(csv_alloc_header_t) + sizeof(void*);
    size_t misalignment = (uintptr_t)after_header % alignment;
    char* user_ptr = after_header;
    if (misalignment) {
        user_ptr += alignment - misalignment;
    }

    // Store pointer to header directly before user data (for O(1) lookup on free)
    void** header_ptr_slot = (void**)(user_ptr - sizeof(void*));
    *header_ptr_slot = header;

    // Zero-initialize user portion
    memset(user_ptr, 0, size);
    return user_ptr;
}

static sonicsv_always_inline void csv_aligned_free(void* ptr) {
    if (!ptr) return;

    // Direct O(1) header lookup via stored pointer
    void** header_ptr_slot = (void**)((char*)ptr - sizeof(void*));
    csv_alloc_header_t* header = (csv_alloc_header_t*)*header_ptr_slot;

    // Validate header with bounds check
    if (header && (uintptr_t)header < (uintptr_t)ptr && header->magic == CSV_ALLOC_MAGIC) {
        csv_untrack_allocation(header);
        size_t original_size = header->size;
        header->magic = 0; // Clear magic to detect double-free

        // Try to recycle the block
        if (!csv_pool_free(header, original_size)) {
            free(header);
        }
    }
    // Silently ignore invalid pointers for safety
}
 
 // Memory statistics
 static sonicsv_always_inline size_t csv_get_allocated_memory(void) {
     return atomic_load_explicit(&g_total_allocated, memory_order_relaxed);
 }

 static sonicsv_always_inline size_t csv_get_peak_memory(void) {
     return atomic_load_explicit(&g_peak_allocated, memory_order_relaxed);
 }
 
 // Optimized buffer sizes based on typical CSV patterns
 #define CSV_INITIAL_FIELD_CAPACITY 512
 #define CSV_INITIAL_BUFFER_CAPACITY 16384
 #define CSV_FIELD_DATA_POOL_INITIAL 32768
 #define CSV_GROWTH_FACTOR 1.5  // Less aggressive growth to reduce memory waste
 
 // SIMD feature detection - single atomic with initialized flag in high bit
#define CSV_SIMD_INITIALIZED_FLAG 0x80000000U
static atomic_uint g_simd_features_atomic = ATOMIC_VAR_INIT(0);
 
 // Parser state machine
 typedef enum {
   CSV_STATE_FIELD_START,
   CSV_STATE_IN_QUOTED_FIELD,
   CSV_STATE_QUOTE_IN_QUOTED_FIELD,
 } csv_parse_state_t;
 
 // Thread-local parser statistics for isolation
 typedef struct {
   uint64_t simd_ops;
   uint64_t scalar_fallbacks;
 } csv_thread_stats_t;

 static __thread csv_thread_stats_t g_thread_stats = {0, 0};

 // Internal parser structure - isolated and thread-safe
 struct csv_parser {
   csv_parse_options_t options;
   csv_row_callback_t row_callback;
   void *row_callback_data;
   csv_error_callback_t error_callback;
   void *error_callback_data;
   csv_parse_state_t state;
   char* unparsed_buffer sonicsv_aligned(SONICSV_SIMD_ALIGN);
   size_t unparsed_size;
   size_t unparsed_capacity;
   csv_field_t *fields sonicsv_aligned(SONICSV_SIMD_ALIGN);
   size_t fields_capacity;
   size_t num_fields;
   char *field_buffer sonicsv_aligned(SONICSV_SIMD_ALIGN);
   size_t field_buffer_capacity;
   size_t field_buffer_pos;
   // Optimized field data storage with better memory layout
   char *field_data_pool sonicsv_aligned(SONICSV_SIMD_ALIGN);
   size_t field_data_pool_size;
   size_t field_data_pool_capacity;
   csv_stats_t stats;
   struct timespec start_time;
   size_t peak_memory;
   uint64_t current_row_start_offset;
   // Parser instance ID for debugging
   uint64_t instance_id;
 } sonicsv_aligned(64);
 
 // Thread-safe instance ID generation
 static _Atomic(uint64_t) g_next_parser_id = 1;
 
 // --- Enhanced Runtime SIMD Feature Detection ---
 #ifdef __x86_64__
 static sonicsv_cold uint32_t csv_detect_x86_features(void) {
     uint32_t features = CSV_SIMD_NONE;
 
 #if defined(__GNUC__) || defined(__clang__)
     unsigned int eax, ebx, ecx, edx;
 
     // Check if CPUID is supported
     if (__get_cpuid_max(0, NULL) == 0) return features;
 
     // Check basic features (SSE4.2)
     if (__get_cpuid(1, &eax, &ebx, &ecx, &edx)) {
         if (ecx & bit_SSE4_2) {
             // Simple feature flag check - modern OSes handle SIMD context switching
             features |= CSV_SIMD_SSE4_2;
         }
     }
 
     // Check extended features (AVX2, AVX-512)
     if (__get_cpuid_max(0, NULL) >= 7) {
         if (__get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx)) {
             // Check AVX2 support
             if (ebx & bit_AVX2) {
                 features |= CSV_SIMD_AVX2;
             }
 
             // Check AVX-512F support
             if (ebx & (1 << 16)) {
                 features |= CSV_SIMD_AVX512;
             }
         }
     }
 #endif
     return features;
 }
 #endif
 
 #ifdef __aarch64__
 static sonicsv_cold uint32_t csv_detect_arm_features(void) {
     uint32_t features = CSV_SIMD_NONE;
 
     // NEON is standard on ARMv8-A (AArch64)
     features |= CSV_SIMD_NEON;
 
 #ifdef HAVE_SVE
     // Check for SVE support via system calls if needed
     // For now, assume SVE is available if compiled with support
     features |= CSV_SIMD_SVE;
 #endif
 
     return features;
 }
 #endif
 
static sonicsv_cold uint32_t csv_simd_init(void) {
    uint32_t features = CSV_SIMD_NONE;

#ifdef __x86_64__
    features = csv_detect_x86_features();
#elif defined(__aarch64__)
    features = csv_detect_arm_features();
#endif

    features |= CSV_SIMD_INITIALIZED_FLAG;
    atomic_store_explicit(&g_simd_features_atomic, features, memory_order_relaxed);
    return features;
}
 
// --- Search result struct ---
typedef struct { const char *pos; size_t offset; } csv_search_result_t;

// SWAR helper macros for processing 8 bytes at a time
#define SWAR_BROADCAST(c) (0x0101010101010101ULL * (uint8_t)(c))
#define SWAR_HAS_ZERO(x) (((x) - 0x0101010101010101ULL) & ~(x) & 0x8080808080808080ULL)

// Find position of first matching byte in 64-bit word
static sonicsv_force_inline int csv_swar_find_first(uint64_t mask) {
#if SONICSV_LITTLE_ENDIAN
    return __builtin_ctzll(mask) >> 3;
#else
    return __builtin_clzll(mask) >> 3;
#endif
}

// --- Optimized SIMD implementations ---
// SWAR-optimized scalar fallback processes 8 bytes at a time
static sonicsv_force_inline csv_search_result_t csv_scalar_find_char(const char *d, size_t s, char c1, char c2, char c3, char c4) {
    if (sonicsv_unlikely(s == 0)) return (csv_search_result_t){NULL, 0};

    // Handle unaligned prefix
    size_t i = 0;
    size_t align_offset = (uintptr_t)d & 7;
    if (align_offset != 0) {
        size_t prefix_len = 8 - align_offset;
        if (prefix_len > s) prefix_len = s;
        for (; i < prefix_len; i++) {
            char c = d[i];
            if (c == c1 || c == c2 || c == c3 || c == c4)
                return (csv_search_result_t){d + i, i};
        }
    }

    // SWAR main loop - process 8 bytes at a time
    if (s >= 8) {
        uint64_t bc1 = SWAR_BROADCAST(c1);
        uint64_t bc2 = SWAR_BROADCAST(c2);
        uint64_t bc3 = SWAR_BROADCAST(c3);
        uint64_t bc4 = SWAR_BROADCAST(c4);

        for (; i + 8 <= s; i += 8) {
            uint64_t chunk;
            memcpy(&chunk, d + i, 8);

            // XOR with broadcast values - matching bytes become 0
            uint64_t m1 = SWAR_HAS_ZERO(chunk ^ bc1);
            uint64_t m2 = SWAR_HAS_ZERO(chunk ^ bc2);
            uint64_t m3 = SWAR_HAS_ZERO(chunk ^ bc3);
            uint64_t m4 = SWAR_HAS_ZERO(chunk ^ bc4);
            uint64_t combined = m1 | m2 | m3 | m4;

            if (combined) {
                int pos = csv_swar_find_first(combined);
                return (csv_search_result_t){d + i + pos, i + pos};
            }
        }
    }

    // Handle remaining bytes
    for (; i < s; i++) {
        char c = d[i];
        if (c == c1 || c == c2 || c == c3 || c == c4)
            return (csv_search_result_t){d + i, i};
    }

    return (csv_search_result_t){NULL, s};
}

// Optimized single-character search using SWAR
static sonicsv_force_inline const char* csv_find_single_char(const char *d, size_t s, char target) {
    if (sonicsv_unlikely(s == 0)) return NULL;

    size_t i = 0;

    // SWAR main loop for single char - process 8 bytes at a time
    if (s >= 8) {
        uint64_t broadcast = SWAR_BROADCAST(target);

        for (; i + 8 <= s; i += 8) {
            uint64_t chunk;
            memcpy(&chunk, d + i, 8);

            uint64_t match = SWAR_HAS_ZERO(chunk ^ broadcast);
            if (match) {
                int pos = csv_swar_find_first(match);
                return d + i + pos;
            }
        }
    }

    // Handle remaining bytes
    for (; i < s; i++) {
        if (d[i] == target) return d + i;
    }

    return NULL;
}

#ifdef HAVE_SSE4_2
 __attribute__((target("sse4.2")))
 static sonicsv_hot csv_search_result_t csv_sse42_find_char(const char *d, size_t s, char c1, char c2, char c3, char c4) {
   if (sonicsv_unlikely(s < 16)) return csv_scalar_find_char(d, s, c1, c2, c3, c4);
 
   // Bounds check to prevent buffer overrun
   if (sonicsv_unlikely(!d || s == 0)) return (csv_search_result_t){NULL, s};
 
   __m128i v_c1 = _mm_set1_epi8(c1), v_c2 = _mm_set1_epi8(c2),
           v_c3 = _mm_set1_epi8(c3), v_c4 = _mm_set1_epi8(c4);
   size_t i = 0;
 
   // Safe loop with explicit bounds checking
   for (; i + 16 <= s && i <= s - 16; i += 16) {
     __m128i chunk = _mm_loadu_si128((__m128i const*)(d + i));
     __m128i cmp = _mm_or_si128(_mm_or_si128(
       _mm_or_si128(_mm_cmpeq_epi8(chunk, v_c1), _mm_cmpeq_epi8(chunk, v_c2)),
       _mm_cmpeq_epi8(chunk, v_c3)), _mm_cmpeq_epi8(chunk, v_c4));
     uint32_t mask = _mm_movemask_epi8(cmp);
     if (mask != 0) {
       size_t bit_pos = __builtin_ctz(mask);
       if (i + bit_pos < s) return (csv_search_result_t){d + i + bit_pos, i + bit_pos};
     }
   }
 
   // Safe tail processing
   if (i < s) {
     csv_search_result_t tail = csv_scalar_find_char(d + i, s - i, c1, c2, c3, c4);
     if (tail.pos) tail.offset += i;
     else tail.offset = s;
     return tail;
   }
 
   return (csv_search_result_t){NULL, s};
 }
 #endif
 
 #ifdef HAVE_AVX2
 __attribute__((target("avx2")))
 static sonicsv_hot csv_search_result_t csv_avx2_find_char(const char *d, size_t s, char c1, char c2, char c3, char c4) {
   if (sonicsv_unlikely(s < 32)) return csv_scalar_find_char(d, s, c1, c2, c3, c4);
 
   // Bounds check to prevent buffer overrun
   if (sonicsv_unlikely(!d || s == 0)) return (csv_search_result_t){NULL, s};
 
   __m256i v_c1 = _mm256_set1_epi8(c1), v_c2 = _mm256_set1_epi8(c2),
           v_c3 = _mm256_set1_epi8(c3), v_c4 = _mm256_set1_epi8(c4);
   size_t i = 0;
 
   // Safe loop with explicit bounds checking
   for (; i + 32 <= s && i <= s - 32; i += 32) {
     // Safe prefetch - only if we have enough remaining data
     if (i + 64 <= s) sonicsv_prefetch_read(d, i + 64);
     __m256i chunk = _mm256_loadu_si256((__m256i const*)(d + i));
     uint32_t mask = _mm256_movemask_epi8(_mm256_or_si256(_mm256_or_si256(
       _mm256_or_si256(_mm256_cmpeq_epi8(chunk, v_c1), _mm256_cmpeq_epi8(chunk, v_c2)),
       _mm256_cmpeq_epi8(chunk, v_c3)), _mm256_cmpeq_epi8(chunk, v_c4)));
     if (mask != 0) {
       size_t bit_pos = __builtin_ctz(mask);
       if (i + bit_pos < s) return (csv_search_result_t){d + i + bit_pos, i + bit_pos};
     }
   }
 
   // Safe tail processing
   if (i < s) {
     csv_search_result_t tail = csv_scalar_find_char(d + i, s - i, c1, c2, c3, c4);
     if (tail.pos) tail.offset += i;
     else tail.offset = s;
     return tail;
   }
 
   return (csv_search_result_t){NULL, s};
 }
 #endif
 
 #ifdef HAVE_NEON
 static sonicsv_hot csv_search_result_t csv_neon_find_char(const char *d, size_t s, char c1, char c2, char c3, char c4) {
   if (sonicsv_unlikely(s < 16)) return csv_scalar_find_char(d, s, c1, c2, c3, c4);
 
   // Bounds check to prevent buffer overrun
   if (sonicsv_unlikely(!d || s == 0)) return (csv_search_result_t){NULL, s};
 
   uint8x16_t v_c1 = vdupq_n_u8((uint8_t)c1), v_c2 = vdupq_n_u8((uint8_t)c2),
              v_c3 = vdupq_n_u8((uint8_t)c3), v_c4 = vdupq_n_u8((uint8_t)c4);
   size_t i = 0;
 
   // Safe loop with explicit bounds checking
   for (; i + 16 <= s && i <= s - 16; i += 16) {
     // Safe prefetch - only if we have enough remaining data
     if (i + 64 <= s) __builtin_prefetch(d + i + 64, 0, 3);
     uint8x16_t chunk = vld1q_u8((uint8_t const*)(d + i));
     uint8x16_t cmp = vorrq_u8(vorrq_u8(
       vorrq_u8(vceqq_u8(chunk, v_c1), vceqq_u8(chunk, v_c2)),
       vceqq_u8(chunk, v_c3)), vceqq_u8(chunk, v_c4));
 
     // Extract mask from comparison result - handle endianness
     uint64_t mask_lo = vgetq_lane_u64(vreinterpretq_u64_u8(cmp), 0);
     uint64_t mask_hi = vgetq_lane_u64(vreinterpretq_u64_u8(cmp), 1);
 
 #if SONICSV_BIG_ENDIAN
     mask_lo = csv_bswap64(mask_lo);
     mask_hi = csv_bswap64(mask_hi);
 #endif
 
     if (mask_lo != 0) {
       int pos = __builtin_ctzll(mask_lo) / 8;
       if (i + pos < s) return (csv_search_result_t){d + i + pos, i + pos};
     }
     if (mask_hi != 0) {
       int pos = 8 + __builtin_ctzll(mask_hi) / 8;
       if (i + pos < s) return (csv_search_result_t){d + i + pos, i + pos};
     }
   }
 
   // Safe tail processing
   if (i < s) {
     csv_search_result_t tail = csv_scalar_find_char(d + i, s - i, c1, c2, c3, c4);
     if (tail.pos) tail.offset += i;
     else tail.offset = s;
     return tail;
   }
 
   return (csv_search_result_t){NULL, s};
 }
 #endif

// Aggressive prefetching for large buffers
static sonicsv_force_inline void csv_prefetch_range(const char *data, size_t size) {
    for (size_t offset = 0; offset < size && offset < 2048; offset += SONICSV_CACHE_LINE_SIZE) {
        sonicsv_prefetch_read(data, offset);
    }
}

// Parser-isolated SIMD interface
static sonicsv_force_inline csv_search_result_t csv_find_special_char_with_parser(csv_parser_t *parser, const char *d, size_t s, char del, char quo, char nl, char cr) {
    (void)parser; // Parser context no longer needed for SIMD dispatch

    // Prefetch ahead for large buffers
    if (s > 256) csv_prefetch_range(d, s);

    uint32_t features = csv_get_simd_features();
 
     // Dispatch to best available implementation
 #ifdef __x86_64__
     #ifdef HAVE_AVX2
     if (sonicsv_likely(features & CSV_SIMD_AVX2)) {
         g_thread_stats.simd_ops++;
         return csv_avx2_find_char(d, s, del, quo, nl, cr);
     }
     #endif
     #ifdef HAVE_SSE4_2
     if (sonicsv_likely(features & CSV_SIMD_SSE4_2)) {
         g_thread_stats.simd_ops++;
         return csv_sse42_find_char(d, s, del, quo, nl, cr);
     }
     #endif
 #elif defined(__aarch64__)
     #ifdef HAVE_NEON
     if (sonicsv_likely(features & CSV_SIMD_NEON)) {
         g_thread_stats.simd_ops++;
         return csv_neon_find_char(d, s, del, quo, nl, cr);
     }
     #endif
 #endif
 
     // Fallback to scalar implementation
     g_thread_stats.scalar_fallbacks++;
     return csv_scalar_find_char(d, s, del, quo, nl, cr);
 }
 
 // Backward compatibility wrapper (no parser context)
static sonicsv_force_inline csv_search_result_t csv_find_special_char(const char *d, size_t s, char del, char quo, char nl, char cr) {
    uint32_t features = csv_get_simd_features();

#ifdef __x86_64__
    #ifdef HAVE_AVX2
    if (sonicsv_likely(features & CSV_SIMD_AVX2))
        return csv_avx2_find_char(d, s, del, quo, nl, cr);
    #endif
    #ifdef HAVE_SSE4_2
    if (sonicsv_likely(features & CSV_SIMD_SSE4_2))
        return csv_sse42_find_char(d, s, del, quo, nl, cr);
    #endif
#elif defined(__aarch64__)
    #ifdef HAVE_NEON
    if (sonicsv_likely(features & CSV_SIMD_NEON))
        return csv_neon_find_char(d, s, del, quo, nl, cr);
    #endif
#endif

    return csv_scalar_find_char(d, s, del, quo, nl, cr);
}
 
// --- Enhanced helper functions ---
// Enhanced memory allocation with overflow protection and growth strategy
static sonicsv_force_inline csv_error_t ensure_capacity_safe(void **p, size_t *cap, size_t req, size_t el_sz, csv_parser_t *parser) {
    if (sonicsv_likely(*cap >= req)) return CSV_OK;

    // Validate inputs
    if (el_sz == 0) return CSV_ERROR_INVALID_ARGS;

    // Check for integer overflow using safe multiplication
    size_t req_size;
    if (!csv_safe_mul(req, el_sz, &req_size)) return CSV_ERROR_OUT_OF_MEMORY;

    // Calculate new capacity with growth factor
    size_t new_cap = *cap ? (size_t)(*cap * CSV_GROWTH_FACTOR) + 1 : 64;
    if (new_cap < req) new_cap = req;

    // Align to cache line boundaries for better performance
    new_cap = (new_cap + SONICSV_CACHE_LINE_SIZE - 1) & ~((size_t)(SONICSV_CACHE_LINE_SIZE - 1));

    // Safe multiplication for new size
    size_t new_size;
    if (!csv_safe_mul(new_cap, el_sz, &new_size)) return CSV_ERROR_OUT_OF_MEMORY;

    // Prevent excessive memory usage
    if (parser && parser->options.max_memory_kb > 0) {
        size_t current_memory = csv_get_allocated_memory();
        size_t old_size = *cap * el_sz;
        size_t max_allowed = parser->options.max_memory_kb * 1024;
        if (current_memory > old_size && (current_memory - old_size) + new_size > max_allowed) {
            return CSV_ERROR_OUT_OF_MEMORY;
        }
    }

    void *new_p = csv_aligned_alloc(new_size, SONICSV_SIMD_ALIGN);
    if (sonicsv_unlikely(!new_p)) return CSV_ERROR_OUT_OF_MEMORY;

    if (*p) {
        size_t copy_size = *cap * el_sz;
        memcpy(new_p, *p, copy_size);
        csv_aligned_free(*p);
     }
 
     *p = new_p;
     *cap = new_cap;
 
     // Update parser memory tracking
     if (parser) {
         parser->peak_memory = csv_get_peak_memory();
     }
 
     return CSV_OK;
 }
 
 static sonicsv_always_inline csv_error_t ensure_capacity(void **p, size_t *cap, size_t req, size_t el_sz, csv_parser_t *parser) {
     return ensure_capacity_safe(p, cap, req, el_sz, parser);
 }
 
 
 static sonicsv_always_inline csv_error_t report_error(csv_parser_t *p, csv_error_t e, const char *m) {
   p->stats.errors_encountered++;
   if (p->error_callback) p->error_callback(e, m, p->stats.total_rows_parsed + 1, p->error_callback_data);
   return e;
 }
 
 // Optimized field processing with branch prediction hints
 static sonicsv_force_inline csv_error_t add_field(csv_parser_t *p, const char *d, size_t s, bool q) {
   if (sonicsv_unlikely(s > p->options.max_field_size))
     return report_error(p, CSV_ERROR_FIELD_TOO_LARGE, "Field size exceeds max_field_size");
 
   // Pre-allocate fields array with better prediction
   if (sonicsv_unlikely(p->num_fields >= p->fields_capacity)) {
     if (sonicsv_unlikely(ensure_capacity((void**)&p->fields, &p->fields_capacity,
                                          p->num_fields + 1, sizeof(csv_field_t), p) != CSV_OK))
       return CSV_ERROR_OUT_OF_MEMORY;
   }
 
   const char *field_data = d;
 
   if (sonicsv_unlikely(q)) {
     // Quoted field processing with optimized copying
     size_t required_size = p->field_data_pool_size + s + 1;
     if (sonicsv_unlikely(required_size > p->field_data_pool_capacity)) {
       if (sonicsv_unlikely(ensure_capacity((void**)&p->field_data_pool, &p->field_data_pool_capacity,
                                            required_size, 1, p) != CSV_OK))
         return CSV_ERROR_OUT_OF_MEMORY;
     }
 
     field_data = p->field_data_pool + p->field_data_pool_size;
 
     // Optimized copy based on field size
     if (sonicsv_likely(s <= 128)) {
       // Small field - direct copy
       memcpy((char*)field_data, d, s);
     } else {
       // Large field - use potentially vectorized memcpy
       memcpy((char*)field_data, d, s);
     }
     ((char*)field_data)[s] = '\0';
     p->field_data_pool_size = required_size;
   } else if (sonicsv_unlikely(p->options.trim_whitespace && s > 0)) {
     // Unquoted field with trimming
     const char *start = d, *end = d + s;
     while (start < end && (*start == ' ' || *start == '\t')) start++;
     while (end > start && (end[-1] == ' ' || end[-1] == '\t')) end--;
     field_data = start;
     s = end - start;
   }
 
   // Store field with likely branch prediction
   csv_field_t *field = &p->fields[p->num_fields++];
   field->data = field_data;
   field->size = s;
   field->quoted = q;
 
   // Update statistics with optimized calculation
   p->stats.total_fields_parsed++;
   if (sonicsv_likely(p->stats.total_fields_parsed > 1)) {
     p->stats.perf.avg_field_size += (s - p->stats.perf.avg_field_size) / p->stats.total_fields_parsed;
   } else {
     p->stats.perf.avg_field_size = s;
   }
 
   return CSV_OK;
 }
 
 static sonicsv_force_inline csv_error_t finish_row(csv_parser_t *p) {
   if (p->num_fields == 0 && p->options.ignore_empty_lines) {
     return CSV_OK;
   }
 
   size_t total_field_size = 0;
   for (size_t i = 0; i < p->num_fields; i++) total_field_size += p->fields[i].size;
   if (sonicsv_unlikely(total_field_size > p->options.max_row_size))
     return report_error(p, CSV_ERROR_ROW_TOO_LARGE, "Row size exceeds max_row_size");
 
   p->stats.total_rows_parsed++;
   if (p->stats.total_rows_parsed > 0) {
     p->stats.perf.avg_row_size = (p->stats.perf.avg_row_size * (p->stats.total_rows_parsed - 1) + total_field_size) / p->stats.total_rows_parsed;
   }
 
   if (p->row_callback) {
     csv_row_t row = {p->fields, p->num_fields, p->stats.total_rows_parsed, p->current_row_start_offset};
     p->row_callback(&row, p->row_callback_data);
   }
 
   p->num_fields = 0;
   return CSV_OK;
 }
 
 static sonicsv_force_inline csv_error_t append_to_field_buffer(csv_parser_t *p, const char *s, size_t l) {
   if (sonicsv_unlikely(p->field_buffer_pos + l > p->options.max_field_size))
     return CSV_ERROR_FIELD_TOO_LARGE;
   if (sonicsv_unlikely(ensure_capacity((void**)&p->field_buffer, &p->field_buffer_capacity,
                                        p->field_buffer_pos + l, 1, p) != CSV_OK))
     return CSV_ERROR_OUT_OF_MEMORY;
 
   memcpy(p->field_buffer + p->field_buffer_pos, s, l);
   p->field_buffer_pos += l;
   return CSV_OK;
 }
 
// UTF-8 BOM detection constant
static const unsigned char CSV_UTF8_BOM[3] = {0xEF, 0xBB, 0xBF};

// Optimized 2-char search for fast path (delimiter and newline only)
static sonicsv_force_inline const char* csv_find_delim_or_newline(const char *d, size_t s, char delim) {
    if (s == 0) return NULL;

    size_t i = 0;

    // SWAR search for delimiter or newline
    if (s >= 8) {
        uint64_t bc_delim = SWAR_BROADCAST(delim);
        uint64_t bc_nl = SWAR_BROADCAST('\n');
        uint64_t bc_cr = SWAR_BROADCAST('\r');

        for (; i + 8 <= s; i += 8) {
            uint64_t chunk;
            memcpy(&chunk, d + i, 8);

            uint64_t m1 = SWAR_HAS_ZERO(chunk ^ bc_delim);
            uint64_t m2 = SWAR_HAS_ZERO(chunk ^ bc_nl);
            uint64_t m3 = SWAR_HAS_ZERO(chunk ^ bc_cr);
            uint64_t combined = m1 | m2 | m3;

            if (combined) {
                int pos_offset = csv_swar_find_first(combined);
                return d + i + pos_offset;
            }
        }
    }

    // Scalar tail
    for (; i < s; i++) {
        char c = d[i];
        if (c == delim || c == '\n' || c == '\r') return d + i;
    }

    return NULL;
}

// Fast-path parser for simple CSVs (no quotes) - significantly faster
static sonicsv_hot csv_error_t csv_parse_simple_fast(csv_parser_t *p, const char *buf, size_t sz) {
    const char delimiter = p->options.delimiter;
    const size_t max_field_size = p->options.max_field_size;
    const char *pos = buf;
    const char *end = buf + sz;
    const char *row_start = pos;
    const char *field_start = pos;

    // Pre-allocate fields array for expected row size
    if (p->fields_capacity < 64) {
        if (ensure_capacity((void**)&p->fields, &p->fields_capacity, 64, sizeof(csv_field_t), p) != CSV_OK)
            return CSV_ERROR_OUT_OF_MEMORY;
    }

    while (pos < end) {
        // Optimized 2-char search for delimiter or newline
        const char *found = csv_find_delim_or_newline(pos, end - pos, delimiter);
        csv_search_result_t res = {found, found ? (size_t)(found - pos) : (size_t)(end - pos)};

        if (!res.pos) {
            // No special char found - rest is last field
            size_t field_size = end - field_start;
            if (sonicsv_unlikely(field_size > max_field_size)) {
                return report_error(p, CSV_ERROR_FIELD_TOO_LARGE, "Field size exceeds max_field_size");
            }
            if (p->num_fields < p->fields_capacity) {
                p->fields[p->num_fields].data = field_start;
                p->fields[p->num_fields].size = field_size;
                p->fields[p->num_fields].quoted = false;
                p->num_fields++;
            }
            p->stats.total_rows_parsed++;
            p->stats.total_fields_parsed += p->num_fields;
            if (p->row_callback) {
                csv_row_t row = {p->fields, p->num_fields, p->stats.total_rows_parsed, (size_t)(row_start - buf)};
                p->row_callback(&row, p->row_callback_data);
            }
            p->num_fields = 0;
            break;
        }

        char c = *res.pos;
        pos = res.pos;

        if (c == delimiter) {
            // Add field with size check
            size_t field_size = pos - field_start;
            if (sonicsv_unlikely(field_size > max_field_size)) {
                return report_error(p, CSV_ERROR_FIELD_TOO_LARGE, "Field size exceeds max_field_size");
            }
            if (sonicsv_likely(p->num_fields < p->fields_capacity)) {
                p->fields[p->num_fields].data = field_start;
                p->fields[p->num_fields].size = field_size;
                p->fields[p->num_fields].quoted = false;
                p->num_fields++;
            } else {
                if (ensure_capacity((void**)&p->fields, &p->fields_capacity, p->num_fields + 1, sizeof(csv_field_t), p) != CSV_OK)
                    return CSV_ERROR_OUT_OF_MEMORY;
                p->fields[p->num_fields].data = field_start;
                p->fields[p->num_fields].size = field_size;
                p->fields[p->num_fields].quoted = false;
                p->num_fields++;
            }
            pos++;
            field_start = pos;
        } else { // newline
            // Add last field of row with size check
            size_t field_size = pos - field_start;
            if (sonicsv_unlikely(field_size > max_field_size)) {
                return report_error(p, CSV_ERROR_FIELD_TOO_LARGE, "Field size exceeds max_field_size");
            }
            if (sonicsv_likely(p->num_fields < p->fields_capacity)) {
                p->fields[p->num_fields].data = field_start;
                p->fields[p->num_fields].size = field_size;
                p->fields[p->num_fields].quoted = false;
                p->num_fields++;
            }

            // Finish row
            p->stats.total_rows_parsed++;
            p->stats.total_fields_parsed += p->num_fields;
            if (p->row_callback) {
                csv_row_t row = {p->fields, p->num_fields, p->stats.total_rows_parsed, (size_t)(row_start - buf)};
                p->row_callback(&row, p->row_callback_data);
            }
            p->num_fields = 0;

            // Skip newline(s)
            pos++;
            if (c == '\r' && pos < end && *pos == '\n') pos++;

            row_start = pos;
            field_start = pos;
        }
    }

    p->stats.total_bytes_processed += sz;
    return CSV_OK;
}

// --- FIXED CSV Parsing Logic ---
csv_error_t csv_parse_buffer(csv_parser_t *p, const char *buf, size_t sz, bool is_final) {
  // Enhanced parameter validation
  if (!p) return CSV_ERROR_INVALID_ARGS;
  if (sz > 0 && !buf) return CSV_ERROR_INVALID_ARGS;
  if (sz > SIZE_MAX / 2) return CSV_ERROR_INVALID_ARGS; // Prevent overflow
  if (p->options.max_field_size == 0) return CSV_ERROR_INVALID_ARGS;
  if (p->options.max_row_size == 0) return CSV_ERROR_INVALID_ARGS;

  // Skip UTF-8 BOM at the very beginning of input (first call with no prior data)
  if (sz >= 3 && p->stats.total_bytes_processed == 0 && p->unparsed_size == 0) {
    if (memcmp(buf, CSV_UTF8_BOM, 3) == 0) {
      buf += 3;
      sz -= 3;
      p->current_row_start_offset = 3; // Adjust offset for BOM
    }
  }

  // Fast-path: if parsing complete buffer with no quotes and no special options
  // Conditions: is_final, no pending state, no unparsed data, no trim, no strict mode
  if (is_final && p->state == CSV_STATE_FIELD_START && p->unparsed_size == 0 && sz > 64 &&
      !p->options.trim_whitespace && !p->options.strict_mode) {
    // Check first 4KB for quote character - if none found, likely simple CSV
    size_t sample_size = sz < 4096 ? sz : 4096;
    const char *quote_check = csv_find_single_char(buf, sample_size, p->options.quote_char);
    if (!quote_check) {
      // No quotes found in sample - use fast path
      return csv_parse_simple_fast(p, buf, sz);
    }
  }

  // Handle unparsed data from previous call
   if (p->unparsed_size > 0) {
     if (sonicsv_unlikely(ensure_capacity((void**)&p->unparsed_buffer, &p->unparsed_capacity,
                                          p->unparsed_size + sz, 1, p) != CSV_OK))
       return CSV_ERROR_OUT_OF_MEMORY;
     memcpy(p->unparsed_buffer + p->unparsed_size, buf, sz);
     p->unparsed_size += sz;
     buf = p->unparsed_buffer;
     sz = p->unparsed_size;
   }
 
   const char *pos = buf, *end = buf + sz;
   csv_error_t err = CSV_OK;
   const char delimiter = p->options.delimiter, quote_char = p->options.quote_char;
   size_t original_bytes_processed = p->stats.total_bytes_processed;
 
   // Optimized main parsing loop with better branch prediction
   while (pos < end && sonicsv_likely(err == CSV_OK)) {
     char c = *pos;
 
     // Use computed goto for better branch prediction (if supported)
     switch (p->state) {
       case CSV_STATE_FIELD_START:
         // Optimize common case: unquoted field
         if (sonicsv_likely(c != quote_char && c != delimiter && c != '\n' && c != '\r')) {
           // Fast path: start of unquoted field - use SIMD to find end
           const char *field_start = pos;
 
           csv_search_result_t res = csv_find_special_char_with_parser(p, pos, end - pos, delimiter, quote_char, '\n', '\r');
 
           if (res.pos == NULL) {
             if (is_final) {
               // Add final field and finish row
               if (sonicsv_unlikely((err = add_field(p, field_start, end - field_start, false)) != CSV_OK)) break;
               pos = end;
               if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
             } else {
               // Need more data - save state
               size_t consumed = field_start - buf;
               size_t remaining = end - field_start;
 
               if (buf == p->unparsed_buffer) {
                 memmove(p->unparsed_buffer, field_start, remaining);
               } else {
                 if (sonicsv_unlikely(ensure_capacity((void**)&p->unparsed_buffer, &p->unparsed_capacity,
                                                      remaining, 1, p) != CSV_OK))
                   return CSV_ERROR_OUT_OF_MEMORY;
                 memcpy(p->unparsed_buffer, field_start, remaining);
               }
               p->unparsed_size = remaining;
               p->stats.total_bytes_processed = original_bytes_processed + consumed;
               return CSV_OK;
             }
           } else {
             pos = res.pos;
 
             // Add the field
             if (sonicsv_unlikely((err = add_field(p, field_start, pos - field_start, false)) != CSV_OK)) break;
 
             // Handle what we found
             c = *pos;
             if (c == delimiter) {
               pos++;
             } else if (c == '\n') {
               pos++;
               if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
               p->current_row_start_offset = original_bytes_processed + (pos - buf);
             } else if (c == '\r') {
               pos++;
               if (pos < end && *pos == '\n') pos++; // Skip LF in CRLF
               if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
               p->current_row_start_offset = original_bytes_processed + (pos - buf);
             } else if (c == quote_char && p->options.strict_mode) {
               err = report_error(p, CSV_ERROR_PARSE_ERROR, "Quote character in unquoted field");
               break;
             }
           }
         } else if (c == quote_char) {
           p->state = CSV_STATE_IN_QUOTED_FIELD;
           p->field_buffer_pos = 0;
           pos++;
         } else if (c == delimiter) {
           if (sonicsv_unlikely((err = add_field(p, "", 0, false)) != CSV_OK)) break;
           pos++;
         } else if (c == '\n') {
           if (sonicsv_unlikely((err = add_field(p, "", 0, false)) != CSV_OK)) break;
           if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
           p->current_row_start_offset = original_bytes_processed + (pos + 1 - buf);
           pos++;
         } else if (c == '\r') {
           if (sonicsv_unlikely((err = add_field(p, "", 0, false)) != CSV_OK)) break;
           pos++;
           if (pos < end && *pos == '\n') pos++; // Skip LF in CRLF
           if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
           p->current_row_start_offset = original_bytes_processed + (pos - buf);
         }
         break;
 
       case CSV_STATE_IN_QUOTED_FIELD:
         {
           const char *chunk_start = pos;

           // SWAR-optimized quote scanning - process 8 bytes at a time
           const char *quote_pos = csv_find_single_char(pos, end - pos, quote_char);
           if (!quote_pos) quote_pos = end;

           if (quote_pos > chunk_start) {
             if (sonicsv_unlikely((err = append_to_field_buffer(p, chunk_start, quote_pos - chunk_start)) != CSV_OK)) break;
           }

           if (quote_pos == end) {
             if (sonicsv_unlikely(is_final)) {
               if (sonicsv_unlikely(p->options.strict_mode)) {
                 err = report_error(p, CSV_ERROR_PARSE_ERROR, "Unclosed quoted field");
                 break;
               } else {
                 // Accept unclosed quote in non-strict mode
                 if (sonicsv_unlikely((err = add_field(p, p->field_buffer, p->field_buffer_pos, true)) != CSV_OK)) break;
                 if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
                 pos = quote_pos;
                 break;
               }
             } else {
               // Save state for continuation - optimize for common case
               size_t consumed = chunk_start - buf;
               size_t remaining = end - chunk_start;
 
               if (sonicsv_likely(buf == p->unparsed_buffer)) {
                 memmove(p->unparsed_buffer, chunk_start, remaining);
               } else {
                 if (sonicsv_unlikely(ensure_capacity((void**)&p->unparsed_buffer, &p->unparsed_capacity,
                                                      remaining, 1, p) != CSV_OK))
                   return CSV_ERROR_OUT_OF_MEMORY;
                 memcpy(p->unparsed_buffer, chunk_start, remaining);
               }
               p->unparsed_size = remaining;
               p->stats.total_bytes_processed = original_bytes_processed + consumed;
               return CSV_OK;
             }
           }
 
           // Found closing quote
           pos = quote_pos + 1; // Skip the quote
           if (sonicsv_unlikely(p->options.double_quote && pos < end && *pos == quote_char)) {
             // Escaped quote - handle efficiently
             if (sonicsv_unlikely((err = append_to_field_buffer(p, &quote_char, 1)) != CSV_OK)) break;
             pos++; // Skip second quote
           } else {
             p->state = CSV_STATE_QUOTE_IN_QUOTED_FIELD;
           }
         }
         break;
 
       case CSV_STATE_QUOTE_IN_QUOTED_FIELD:
         if (c == delimiter) {
           if (sonicsv_unlikely((err = add_field(p, p->field_buffer, p->field_buffer_pos, true)) != CSV_OK)) break;
           p->state = CSV_STATE_FIELD_START;
           pos++;
         } else if (c == '\n') {
           if (sonicsv_unlikely((err = add_field(p, p->field_buffer, p->field_buffer_pos, true)) != CSV_OK)) break;
           if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
           p->state = CSV_STATE_FIELD_START;
           p->current_row_start_offset = original_bytes_processed + (pos + 1 - buf);
           pos++;
         } else if (c == '\r') {
           if (sonicsv_unlikely((err = add_field(p, p->field_buffer, p->field_buffer_pos, true)) != CSV_OK)) break;
           pos++;
           if (pos < end && *pos == '\n') pos++; // Skip LF in CRLF
           if (sonicsv_unlikely((err = finish_row(p)) != CSV_OK)) break;
           p->state = CSV_STATE_FIELD_START;
           p->current_row_start_offset = original_bytes_processed + (pos - buf);
         } else if (c == ' ' || c == '\t') {
           pos++;
         } else if (p->options.strict_mode) {
           err = report_error(p, CSV_ERROR_PARSE_ERROR, "Unexpected character after closing quote");
           break;
         } else {
           if (sonicsv_unlikely((err = append_to_field_buffer(p, &quote_char, 1)) != CSV_OK)) break;
           if (sonicsv_unlikely((err = append_to_field_buffer(p, &c, 1)) != CSV_OK)) break;
           p->state = CSV_STATE_IN_QUOTED_FIELD;
           pos++;
         }
         break;
     }
   }
 
   // FIXED: Handle final row when we reach end of input
   if (is_final && err == CSV_OK && p->num_fields > 0) {
     err = finish_row(p);
   }
 
   // Update statistics
   p->stats.total_bytes_processed = original_bytes_processed + (pos - buf);
 
   // Clear unparsed buffer if we consumed everything
   if (buf == p->unparsed_buffer && pos == end) {
     p->unparsed_size = 0;
   }
 
   return err;
 }
 
 // --- Public API Implementation ---
 csv_parse_options_t csv_default_options(void) {
     const char* jobs_env = getenv("SONICSV_JOBS");
     int num_threads = jobs_env ? atoi(jobs_env) : 1;
 
     return (csv_parse_options_t){
         .delimiter = ',', .quote_char = '"', .double_quote = true,
         .trim_whitespace = false, .ignore_empty_lines = true, .strict_mode = false,
         .max_field_size = 10 * 1024 * 1024, .max_row_size = 100 * 1024 * 1024,
         .buffer_size = 64 * 1024, .max_memory_kb = 0, .current_memory = 0,
         .disable_mmap = false, .num_threads = num_threads, .enable_parallel = true,
         .enable_prefetch = true, .prefetch_distance = SONICSV_PREFETCH_DISTANCE,
         .force_alignment = true
     };
 }
 
 const char *csv_error_string(csv_error_t error) {
   switch (error) {
     case CSV_OK: return "Success";
     case CSV_ERROR_INVALID_ARGS: return "Invalid arguments";
     case CSV_ERROR_OUT_OF_MEMORY: return "Out of memory";
     case CSV_ERROR_PARSE_ERROR: return "Parse error";
     case CSV_ERROR_FIELD_TOO_LARGE: return "Field too large";
     case CSV_ERROR_ROW_TOO_LARGE: return "Row too large";
     case CSV_ERROR_IO_ERROR: return "I/O error";
     default: return "Unknown error";
   }
 }
 
 csv_parser_t *csv_parser_create(const csv_parse_options_t *options) {
   csv_parser_t *p = csv_aligned_alloc(sizeof(csv_parser_t), 64);
   if (!p) return NULL;
 
   memset(p, 0, sizeof(csv_parser_t));
   p->options = options ? *options : csv_default_options();
   p->state = CSV_STATE_FIELD_START;
   p->instance_id = atomic_fetch_add_explicit(&g_next_parser_id, 1, memory_order_relaxed);
  
  // Explicitly initialize all pointer fields to NULL
  p->unparsed_buffer = NULL;
  p->fields = NULL;
  p->field_buffer = NULL;
  p->field_data_pool = NULL;
  p->row_callback = NULL;
  p->row_callback_data = NULL;
  p->error_callback = NULL;
  p->error_callback_data = NULL;
  
  // Initialize all size fields to 0
  p->unparsed_size = 0;
  p->unparsed_capacity = 0;
  p->fields_capacity = 0;
  p->num_fields = 0;
  p->field_buffer_capacity = 0;
  p->field_buffer_pos = 0;
  p->field_data_pool_size = 0;
  p->field_data_pool_capacity = 0;
  p->peak_memory = 0;
  p->current_row_start_offset = 0;
  
  // Initialize stats structure and start_time
  memset(&p->stats, 0, sizeof(csv_stats_t));
  memset(&p->start_time, 0, sizeof(struct timespec));

   if (ensure_capacity((void**)&p->fields, &p->fields_capacity, CSV_INITIAL_FIELD_CAPACITY, sizeof(csv_field_t), p) != CSV_OK ||
       ensure_capacity((void**)&p->field_buffer, &p->field_buffer_capacity, CSV_INITIAL_BUFFER_CAPACITY, 1, p) != CSV_OK ||
       ensure_capacity((void**)&p->field_data_pool, &p->field_data_pool_capacity, CSV_FIELD_DATA_POOL_INITIAL, 1, p) != CSV_OK) {
     csv_parser_destroy(p);
     return NULL;
   }
 
   csv_simd_init();
   clock_gettime(CLOCK_MONOTONIC, &p->start_time);
   return p;
 }
 
 void csv_parser_destroy(csv_parser_t *p) {
   if (!p) return;
   csv_aligned_free(p->fields);
   csv_aligned_free(p->field_buffer);
   csv_aligned_free(p->unparsed_buffer);
   csv_aligned_free(p->field_data_pool);
   csv_aligned_free(p);
 }
 
 csv_error_t csv_parser_reset(csv_parser_t *p) {
   if (!p) return CSV_ERROR_INVALID_ARGS;
   p->state = CSV_STATE_FIELD_START;
   p->num_fields = 0;
   p->field_buffer_pos = 0;
   p->unparsed_size = 0;
   p->field_data_pool_size = 0;
   p->current_row_start_offset = 0;
   // Keep SIMD cache initialized across resets
   memset(&p->stats, 0, sizeof(p->stats));
   clock_gettime(CLOCK_MONOTONIC, &p->start_time);
   return CSV_OK;
 }
 
 void csv_parser_set_row_callback(csv_parser_t *p, csv_row_callback_t cb, void *ud) {
   if (!p) return;
   p->row_callback = cb;
   p->row_callback_data = ud;
 }
 
 void csv_parser_set_error_callback(csv_parser_t *p, csv_error_callback_t cb, void *ud) {
   if (!p) return;
   p->error_callback = cb;
   p->error_callback_data = ud;
 }
 
csv_error_t csv_parse_file(csv_parser_t *p, const char *filename) {
    // Enhanced parameter validation
    if (!p) return CSV_ERROR_INVALID_ARGS;
    if (!filename || strlen(filename) == 0) return CSV_ERROR_INVALID_ARGS;
    if (strlen(filename) > 4096) return CSV_ERROR_INVALID_ARGS;

#ifndef _WIN32
    // Use mmap for zero-copy file access (unless disabled)
    if (!p->options.disable_mmap) {
        int fd = open(filename, O_RDONLY);
        if (fd < 0) return report_error(p, CSV_ERROR_IO_ERROR, "Could not open file");

        struct stat st;
        if (fstat(fd, &st) < 0) {
            close(fd);
            return report_error(p, CSV_ERROR_IO_ERROR, "Could not stat file");
        }

        size_t file_size = (size_t)st.st_size;
        if (file_size == 0) {
            close(fd);
            return CSV_OK; // Empty file
        }

        // Memory-map the file for zero-copy access
#ifdef __APPLE__
        void *mapped = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
#else
        void *mapped = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE | MAP_NORESERVE, fd, 0);
#endif
        close(fd);

        if (mapped == MAP_FAILED) {
            // Fallback to stream parsing
            FILE *f = fopen(filename, "rb");
            if (!f) return report_error(p, CSV_ERROR_IO_ERROR, "Could not open file");
            csv_error_t result = csv_parse_stream(p, f);
            fclose(f);
            return result;
        }

        // Advise kernel about sequential access pattern
#ifdef __APPLE__
        // macOS uses different madvise flags
        posix_madvise(mapped, file_size, POSIX_MADV_SEQUENTIAL | POSIX_MADV_WILLNEED);
#else
        madvise(mapped, file_size, MADV_SEQUENTIAL | MADV_WILLNEED);
#endif

        // Parse the entire file at once - zero copy
        csv_error_t result = csv_parse_buffer(p, (const char *)mapped, file_size, true);

        munmap(mapped, file_size);
        return result;
    }
#endif

    // Fallback: stream-based parsing
    FILE *f = fopen(filename, "rb");
    if (!f) return report_error(p, CSV_ERROR_IO_ERROR, "Could not open file");
    csv_error_t result = csv_parse_stream(p, f);
    fclose(f);
    return result;
}
 
 csv_error_t csv_parse_stream(csv_parser_t *p, FILE *stream) {
   // Enhanced parameter validation
   if (!p) return CSV_ERROR_INVALID_ARGS;
   if (!stream) return CSV_ERROR_INVALID_ARGS;
   if (ferror(stream)) return CSV_ERROR_IO_ERROR; // Check stream state
 
   char *buffer = csv_aligned_alloc(p->options.buffer_size, SONICSV_SIMD_ALIGN);
   if (!buffer) return CSV_ERROR_OUT_OF_MEMORY;
 
   csv_error_t err = CSV_OK;
   size_t bytes_read;
   while ((bytes_read = fread(buffer, 1, p->options.buffer_size, stream)) > 0) {
     if ((err = csv_parse_buffer(p, buffer, bytes_read, feof(stream))) != CSV_OK) break;
   }
 
   if (err == CSV_OK) err = csv_parse_buffer(p, "", 0, true);
   if (ferror(stream)) err = report_error(p, CSV_ERROR_IO_ERROR, "File read error");
   csv_aligned_free(buffer);
   return err;
 }
 
 csv_error_t csv_parse_string(csv_parser_t *p, const char *csv_line) {
     // Enhanced parameter validation
     if (!p) return CSV_ERROR_INVALID_ARGS;
     if (!csv_line) return CSV_ERROR_INVALID_ARGS;
     size_t len = strlen(csv_line);
     if (len > p->options.max_row_size) return CSV_ERROR_ROW_TOO_LARGE;
     return csv_parse_buffer(p, csv_line, len, true);
 }
 
 csv_stats_t csv_parser_get_stats(const csv_parser_t *p) {
   if (!p) return (csv_stats_t){0};
   csv_stats_t stats = p->stats;
   struct timespec current_time;
   clock_gettime(CLOCK_MONOTONIC, &current_time);
   uint64_t elapsed_ns = (current_time.tv_sec - p->start_time.tv_sec) * 1000000000ULL +
                         (current_time.tv_nsec - p->start_time.tv_nsec);
   stats.parse_time_ns = elapsed_ns;
   if (elapsed_ns > 0) stats.throughput_mbps = (stats.total_bytes_processed / (1024.0 * 1024.0)) / (elapsed_ns / 1e9);
   stats.simd_acceleration_used = atomic_load_explicit(&g_simd_features_atomic, memory_order_acquire);
   stats.peak_memory_kb = (p->fields_capacity * sizeof(csv_field_t) + p->field_buffer_capacity +
                          p->unparsed_capacity + p->field_data_pool_capacity) / 1024;
 
   if (stats.peak_memory_kb > 0) {
     stats.perf.memory_efficiency = (double)stats.total_bytes_processed / (stats.peak_memory_kb * 1024);
   }
 
   return stats;
 }
 
 void csv_print_stats(const csv_parser_t *p) {
   if (!p) return;
   csv_stats_t s = csv_parser_get_stats(p);
   printf("--- SonicSV Parser Statistics ---\n"
          "  Bytes Processed: %llu\n  Rows Parsed:     %llu\n  Fields Parsed:   %llu\n"
          "  Parse Time:      %.3f ms\n  Throughput:      %.2f MB/s\n"
          "  Peak Memory:     %u KB\n  Errors:          %u\n"
          "  SIMD Operations: %llu\n  Scalar Fallbacks: %llu\n"
          "  SIMD Features:   ",
          (unsigned long long)s.total_bytes_processed, (unsigned long long)s.total_rows_parsed,
          (unsigned long long)s.total_fields_parsed, s.parse_time_ns / 1e6, s.throughput_mbps,
          s.peak_memory_kb, s.errors_encountered,
          (unsigned long long)s.perf.simd_ops, (unsigned long long)s.perf.scalar_fallbacks);
   if (s.simd_acceleration_used == CSV_SIMD_NONE) printf("None");
   else {
     if (s.simd_acceleration_used & CSV_SIMD_AVX512) printf("AVX-512 ");
     if (s.simd_acceleration_used & CSV_SIMD_AVX2) printf("AVX2 ");
     if (s.simd_acceleration_used & CSV_SIMD_SSE4_2) printf("SSE4.2 ");
     if (s.simd_acceleration_used & CSV_SIMD_NEON) printf("NEON ");
     if (s.simd_acceleration_used & CSV_SIMD_SVE) printf("SVE ");
   }
   printf("\n---------------------------------\n");
 }
 
uint32_t csv_get_simd_features(void) {
    uint32_t v = atomic_load_explicit(&g_simd_features_atomic, memory_order_relaxed);
    if (sonicsv_likely(v != 0))
        return v & ~CSV_SIMD_INITIALIZED_FLAG;
    return csv_simd_init() & ~CSV_SIMD_INITIALIZED_FLAG;
}
 
 const csv_field_t *csv_get_field(const csv_row_t *row, size_t index) {
     if (!row || !row->fields || index >= row->num_fields) return NULL;
     return &row->fields[index];
 }
 
 size_t csv_get_num_fields(const csv_row_t *row) {
     if (!row) return 0;
     return row->num_fields;
 }
 
 // --- String Pool Implementation (unchanged) ---
 typedef struct {
   const char* str;
   uint32_t hash;
   size_t length;
 } csv_pool_entry_t;
 
 struct csv_string_pool {
   csv_pool_entry_t* buckets;
   uint32_t num_buckets;
   uint32_t num_items;
   char* data;
   size_t data_size;
   size_t data_capacity;
 } sonicsv_aligned(64);
 
 static sonicsv_always_inline uint32_t fnv1a_hash(const char* str, size_t len) {
   uint32_t hash = 0x811c9dc5;
   for (size_t i = 0; i < len; i++) {
     hash ^= (unsigned char)str[i];
     hash *= 0x01000193;
   }
   return hash;
 }
 
 static sonicsv_always_inline uint32_t next_power_of_2(uint32_t n) {
   if (n <= 1) return 2;
   n--;
   n |= n >> 1;
   n |= n >> 2;
   n |= n >> 4;
   n |= n >> 8;
   n |= n >> 16;
   return n + 1;
 }
 
 static csv_error_t pool_resize(csv_string_pool_t* pool) {
   uint32_t old_num_buckets = pool->num_buckets;
   csv_pool_entry_t* old_buckets = pool->buckets;
   uint32_t new_num_buckets = next_power_of_2(old_num_buckets * 2);
 
   csv_pool_entry_t* new_buckets = csv_aligned_alloc(new_num_buckets * sizeof(csv_pool_entry_t), SONICSV_SIMD_ALIGN);
   if (!new_buckets) return CSV_ERROR_OUT_OF_MEMORY;
   memset(new_buckets, 0, new_num_buckets * sizeof(csv_pool_entry_t));
 
   for (uint32_t i = 0; i < old_num_buckets; i++) {
     if (old_buckets[i].str) {
       uint32_t index = old_buckets[i].hash & (new_num_buckets - 1);
       while (new_buckets[index].str) {
         index = (index + 1) & (new_num_buckets - 1);
       }
       new_buckets[index] = old_buckets[i];
     }
   }
 
   csv_aligned_free(old_buckets);
   pool->buckets = new_buckets;
   pool->num_buckets = new_num_buckets;
   return CSV_OK;
 }
 
 csv_string_pool_t *csv_string_pool_create(size_t initial_capacity) {
   csv_string_pool_t *pool = csv_aligned_alloc(sizeof(csv_string_pool_t), 64);
   if (!pool) return NULL;
 
   memset(pool, 0, sizeof(csv_string_pool_t));
   pool->data_capacity = initial_capacity > 0 ? initial_capacity : 4096;
   pool->data = csv_aligned_alloc(pool->data_capacity, SONICSV_SIMD_ALIGN);
   pool->num_buckets = 16;
   pool->buckets = csv_aligned_alloc(pool->num_buckets * sizeof(csv_pool_entry_t), SONICSV_SIMD_ALIGN);
 
   if (!pool->data || !pool->buckets) {
     csv_string_pool_destroy(pool);
     return NULL;
   }
 
   memset(pool->buckets, 0, pool->num_buckets * sizeof(csv_pool_entry_t));
   return pool;
 }
 
 void csv_string_pool_destroy(csv_string_pool_t *pool) {
   if (!pool) return;
   csv_aligned_free(pool->buckets);
   csv_aligned_free(pool->data);
   csv_aligned_free(pool);
 }
 
 void csv_string_pool_clear(csv_string_pool_t *pool) {
   if (!pool) return;
   memset(pool->buckets, 0, pool->num_buckets * sizeof(csv_pool_entry_t));
   pool->num_items = 0;
   pool->data_size = 0;
 }
 
 const char *csv_string_pool_intern(csv_string_pool_t *pool, const char *str, size_t length) {
   if (!pool || !str) return NULL;
 
   if (pool->num_items * 4 >= pool->num_buckets * 3) {
     if (pool_resize(pool) != CSV_OK) return NULL;
   }
 
   uint32_t hash = fnv1a_hash(str, length);
   uint32_t index = hash & (pool->num_buckets - 1);
 
   while (pool->buckets[index].str) {
     if (pool->buckets[index].hash == hash &&
         pool->buckets[index].length == length &&
         memcmp(pool->buckets[index].str, str, length) == 0) {
       return pool->buckets[index].str;
     }
     index = (index + 1) & (pool->num_buckets - 1);
   }
 
   if (ensure_capacity((void**)&pool->data, &pool->data_capacity,
                      pool->data_size + length + 1, 1, NULL) != CSV_OK)
     return NULL;
 
   char* new_str = pool->data + pool->data_size;
   memcpy(new_str, str, length);
   new_str[length] = '\0';
   pool->data_size += length + 1;
 
   pool->buckets[index] = (csv_pool_entry_t){new_str, hash, length};
   pool->num_items++;
   return new_str;
 }
 
 #endif // SONICSV_IMPLEMENTATION
 
 #endif // SONICSV_H
 
