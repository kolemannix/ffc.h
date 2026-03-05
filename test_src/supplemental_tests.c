#include "ffc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fenv.h>
#include <stdint.h>
#include <errno.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <dirent.h>
#include <sys/stat.h>
#endif

const char* round_name(int d) {
    switch (d) {
        case FE_UPWARD: return "FE_UPWARD";
        case FE_DOWNWARD: return "FE_DOWNWARD";
        case FE_TOWARDZERO: return "FE_TOWARDZERO";
        case FE_TONEAREST: return "FE_TONEAREST";
        default: return "UNKNOWN";
    }
}

// return 1 on success, 0 on failure
int check_file(const char* file_name) {
    printf("Checking %s\n", file_name);
    // We check all rounding directions, for each file.
    int directions[] = {FE_UPWARD, FE_DOWNWARD, FE_TOWARDZERO, FE_TONEAREST};
    size_t num_directions = sizeof(directions) / sizeof(directions[0]);
    for (size_t i = 0; i < num_directions; i++) {
        int d = directions[i];
        fesetround(d);
        size_t number = 0;
        FILE* file = fopen(file_name, "r");
        if (file) {
            char str[4096];
            while (fgets(str, sizeof(str), file)) {
                size_t len = strlen(str);
                if (len > 0 && str[len-1] == '\n') str[len-1] = '\0';
                if (strlen(str) > 0) {
                    // Skip float16 for now, as C support may vary
                    // Read 32-bit hex
                    uint32_t float32 = 0;
                    if (sscanf(str + 5, "%x", &float32) != 1) {
                        fprintf(stderr, "32-bit parsing failure: %s\n", str);
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    // Read 64-bit hex
                    uint64_t float64 = 0;
                    if (sscanf(str + 14, "%llx", &float64) != 1) {
                        fprintf(stderr, "64-bit parsing failure: %s\n", str);
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    // The string to parse:
                    const char* number_string = str + 31;
                    size_t str_len = strlen(number_string);
                    // Parse as 32-bit float
                    float parsed_32;
                    ffc_result result32 = ffc_parse_float(str_len, number_string, &parsed_32);
                    if (result32.outcome != FFC_OUTCOME_OK && result32.outcome != FFC_OUTCOME_OUT_OF_RANGE) {
                        fprintf(stderr, "32-bit ffc parsing failure: %s\n", str);
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    // Parse as 64-bit float
                    double parsed_64;
                    ffc_result result64 = ffc_parse_double(str_len, number_string, &parsed_64);
                    if (result64.outcome != FFC_OUTCOME_OK && result64.outcome != FFC_OUTCOME_OUT_OF_RANGE) {
                        fprintf(stderr, "64-bit ffc parsing failure: %s\n", str);
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    // Convert the floats to unsigned ints.
                    uint32_t float32_parsed = 0;
                    uint64_t float64_parsed = 0;
                    memcpy(&float32_parsed, &parsed_32, sizeof(parsed_32));
                    memcpy(&float64_parsed, &parsed_64, sizeof(parsed_64));
                    // Compare with expected results
                    if (float32_parsed != float32) {
                        printf("bad 32: %s\n", str);
                        printf("parsed as %f\n", parsed_32);
                        printf("as raw uint32_t, parsed = %u, expected = %u\n", float32_parsed, float32);
                        printf("fesetround: %s\n", round_name(d));
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    if (float64_parsed != float64) {
                        printf("bad 64: %s\n", str);
                        printf("parsed as %f\n", parsed_64);
                        printf("as raw uint64_t, parsed = %llu, expected = %llu\n", (unsigned long long)float64_parsed, (unsigned long long)float64);
                        printf("fesetround: %s\n", round_name(d));
                        fclose(file);
                        fesetround(FE_TONEAREST);
                        return 0;
                    }
                    number++;
                }
            }
            printf("checked %zu values\n", number);
            fclose(file);
        } else {
            printf("Could not read %s\n", file_name);
            fesetround(FE_TONEAREST);
            return 0;
        }
    }
    fesetround(FE_TONEAREST);
    return 1;
}

int main(void) {
    int all_passed = 1;
#ifdef _WIN32
    char search_path[1024];
    snprintf(search_path, sizeof(search_path), "%s\\*", SUPPLEMENTAL_TEST_DATA_DIR);
    WIN32_FIND_DATAA ffd;
    HANDLE hFind = FindFirstFileA(search_path, &ffd);
    if (hFind == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "Could not open directory %s\n", SUPPLEMENTAL_TEST_DATA_DIR);
        return 1;
    }
    do {
        if (ffd.cFileName[0] == '.') continue;
        if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) continue;
        char file_path[1024];
        snprintf(file_path, sizeof(file_path), "%s\\%s", SUPPLEMENTAL_TEST_DATA_DIR, ffd.cFileName);
        printf("Testing file: %s\n", file_path);
        if (!check_file(file_path)) {
            all_passed = 0;
        }
    } while (FindNextFileA(hFind, &ffd) != 0);
    FindClose(hFind);
#else
    DIR* dir = opendir(SUPPLEMENTAL_TEST_DATA_DIR);
    if (!dir) {
        fprintf(stderr, "Could not open directory %s: %s\n", SUPPLEMENTAL_TEST_DATA_DIR, strerror(errno));
        return 1;
    }
    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (entry->d_name[0] == '.') continue;
        char file_path[1024];
        snprintf(file_path, sizeof(file_path), "%s/%s", SUPPLEMENTAL_TEST_DATA_DIR, entry->d_name);
        struct stat st;
        if (stat(file_path, &st) == 0 && S_ISREG(st.st_mode)) {
            printf("Testing file: %s\n", file_path);
            if (!check_file(file_path)) {
                all_passed = 0;
            }
        }
    }
    closedir(dir);
#endif
    return all_passed ? EXIT_SUCCESS : EXIT_FAILURE;
}