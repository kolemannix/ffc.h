cmake_minimum_required(VERSION 3.15)

project(ffc VERSION 1.0.0 LANGUAGES C)

include(FetchContent)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Header-only library
add_library(ffc INTERFACE)
add_library(ffc::ffc ALIAS ffc)

target_include_directories(ffc INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include>
)

# Internal header library from src/
add_library(ffc_src INTERFACE)
target_include_directories(ffc_src INTERFACE src/)

# Supplemental test data
add_library(supplemental-data INTERFACE)

# Install library and header
install(TARGETS ffc
    EXPORT ffcTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(EXPORT ffcTargets
    FILE ffcTargets.cmake
    NAMESPACE ffc::
    DESTINATION lib/cmake/ffc
)

install(FILES ffc.h DESTINATION include)

# Tests
enable_testing()

# Copy CSV test files to build directory
file(GLOB csv_files test_src/*.csv)
file(COPY ${csv_files} DESTINATION ${CMAKE_BINARY_DIR}/test_src/)

if(NOT WIN32)
add_executable(test_runner test_src/test.c)
target_compile_definitions(test_runner PRIVATE FFC_IMPL
    _POSIX_C_SOURCE=200809L _DEFAULT_SOURCE)
target_include_directories(test_runner PRIVATE . test_src)
target_link_libraries(test_runner PRIVATE m)

add_test(NAME test_runner COMMAND test_runner)
endif()

add_executable(test_int_runner test_src/test_int.c)
target_compile_definitions(test_int_runner PRIVATE FFC_IMPL)
target_include_directories(test_int_runner PRIVATE . test_src)
target_link_libraries(test_int_runner PRIVATE $<$<NOT:$<C_COMPILER_ID:MSVC>>:m>)

add_test(NAME test_int_runner COMMAND test_int_runner)

# Tests using src/ headers
if(NOT WIN32)
add_executable(test_runner_src test_src/test.c)
target_compile_definitions(test_runner_src PRIVATE FFC_IMPL
    _POSIX_C_SOURCE=200809L _DEFAULT_SOURCE)
target_include_directories(test_runner_src PRIVATE test_src)
target_link_libraries(test_runner_src PRIVATE ffc_src m)

add_test(NAME test_runner_src COMMAND test_runner_src)
endif()

add_executable(test_int_runner_src test_src/test_int.c)
target_compile_definitions(test_int_runner_src PRIVATE FFC_IMPL)
target_include_directories(test_int_runner_src PRIVATE test_src)
target_link_libraries(test_int_runner_src PRIVATE ffc_src $<$<NOT:$<C_COMPILER_ID:MSVC>>:m>)

add_test(NAME test_int_runner_src COMMAND test_int_runner_src)

# Example
add_executable(example example.c)
target_include_directories(example PRIVATE .)


FetchContent_Declare(supplemental_test_files
  GIT_REPOSITORY https://github.com/fastfloat/supplemental_test_files.git
  GIT_TAG origin/main)
message(STATUS "Retrieving test files.")
FetchContent_MakeAvailable(supplemental_test_files)
message(STATUS "Supplemental test files retrieved.")
target_compile_definitions(supplemental-data INTERFACE SUPPLEMENTAL_TEST_DATA_DIR="${supplemental_test_files_BINARY_DIR}/data")



# Supplemental tests
add_executable(supplemental_tests test_src/supplemental_tests.c)
target_include_directories(supplemental_tests PRIVATE . test_src)
target_link_libraries(supplemental_tests PRIVATE $<$<NOT:$<C_COMPILER_ID:MSVC>>:m> supplemental-data)
target_compile_definitions(supplemental_tests PRIVATE FFC_IMPL)

add_test(NAME supplemental_tests COMMAND supplemental_tests)

# Supplemental tests using src/ headers
add_executable(supplemental_tests_src test_src/supplemental_tests.c)
target_include_directories(supplemental_tests_src PRIVATE src/ test_src)
target_link_libraries(supplemental_tests_src PRIVATE $<$<NOT:$<C_COMPILER_ID:MSVC>>:m> supplemental-data)
target_compile_definitions(supplemental_tests_src PRIVATE FFC_IMPL)

add_test(NAME supplemental_tests_src COMMAND supplemental_tests_src)
