cmake_minimum_required(VERSION 3.22)
project(bit-manipulation)

find_package(GTest REQUIRED)
enable_testing()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

set(WARNING_OPTIONS -Wall -Wimplicit-fallthrough -Wpedantic -Wextra -Werror -Wconversion -Wno-bit-int-extension)
set(SANITIZER_OPTIONS
-fsanitize=address,undefined
)

set(INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}" "${CMAKE_CURRENT_LIST_DIR}/src")

set(HEADERS
    src/bmd/bms_to_html.hpp
    src/bmd/directive_type.hpp
    src/bmd/doc_to_html.hpp
    src/bmd/fwd.hpp
    src/bmd/parse.hpp
    src/bmd/html_writer.hpp
    src/bmd/scope_success_writer.hpp

    src/bms/analysis_error.hpp
    src/bms/analyze.hpp
    src/bms/ast.hpp
    src/bms/astp.hpp
    src/bms/basic_diagnostic_consumer.hpp
    src/bms/builtin_function.hpp
    src/bms/comparison_failure.hpp
    src/bms/concrete_type.hpp
    src/bms/concrete_value.hpp
    src/bms/deduction.hpp
    src/bms/diagnostic_consumer.hpp
    src/bms/evaluation_error.hpp
    src/bms/execution_error.hpp
    src/bms/grammar.hpp
    src/bms/linear_map_stack.hpp
    src/bms/operations.hpp
    src/bms/parse_error.hpp
    src/bms/parse.hpp
    src/bms/symbol_table.hpp
    src/bms/tokens.hpp
    src/bms/tokenize_error.hpp
    src/bms/tokenize.hpp
    src/bms/token_type.hpp
    src/bms/value.hpp
    src/bms/vm_codegen.hpp
    src/bms/vm_instructions.hpp
    src/bms/vm.hpp

    src/common/ansi.hpp
    src/common/assert.hpp
    src/common/config.hpp
    src/common/diagnostics.hpp
    src/common/fwd.hpp
    src/common/glue.hpp
    src/common/io.hpp
    src/common/io_error.hpp
    src/common/packs.hpp
    src/common/parse.hpp
    src/common/source_position.hpp
    src/common/result.hpp
    src/common/variant.hpp
)

add_library(bm STATIC ${HEADERS}
    src/bmd/bms_to_html.cpp
    src/bmd/doc_to_html.cpp
    src/bmd/enums.cpp
    src/bmd/html_writer.cpp
    src/bmd/parse.cpp

    src/bms/analyze_name_lookup.cpp
    src/bms/analyze.cpp
    src/bms/ast.cpp
    src/bms/operations.cpp
    src/bms/parse.cpp
    src/bms/tokenize.cpp
    src/bms/util.cpp
    src/bms/vm_codegen.cpp
    src/bms/vm.cpp

    src/common/diagnostics.cpp
    src/common/glue.cpp
    src/common/io.cpp
    src/common/parse.cpp
)

target_compile_options(bm PUBLIC ${WARNING_OPTIONS} ${SANITIZER_OPTIONS})
target_link_options(bm PUBLIC ${SANITIZER_OPTIONS})

add_executable(bm-cli ${HEADERS}
    src/cli/main.cpp
)

add_executable(bm-test ${HEADERS}
    src/test/compilation_stage.hpp
    src/test/diagnostic_policy.hpp
    src/test/main.cpp
    src/test/program_file_testing.cpp
    src/test/variant.cpp
)

include_directories(${INCLUDE_DIRS})

target_link_libraries(bm-cli bm)
target_link_libraries(bm-test bm GTest::GTest GTest::Main)
add_test(NAME MyTest COMMAND bm-test)
