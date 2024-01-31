cmake_minimum_required(VERSION 3.22)
project(bit-manipulation)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

set(WARNING_OPTIONS -Wall -Wpedantic -Wextra -Werror -Wconversion -Wno-bit-int-extension)

set(HEADERS
    src/assert.hpp
    src/bms/bms.hpp
    src/bms/operations.hpp
    src/bms/tokens.hpp
    src/bms/value.hpp
    src/config.hpp
    src/io.hpp)

add_executable(bmscript ${HEADERS}
    src/assert.cpp
    src/bms/analyze.cpp
    src/bms/operations.cpp
    src/bms/parse.cpp
    src/bms/tokenize.cpp
    src/bms/util.cpp
    src/io.cpp
    src/main.cpp)

target_compile_options(bmscript PRIVATE ${WARNING_OPTIONS})
target_include_directories(bmscript PRIVATE "${CMAKE_CURRENT_LIST_DIR}")
target_include_directories(bmscript PRIVATE "${CMAKE_CURRENT_LIST_DIR}/src")

# add_executable(test ${HEADERS} )
