cmake_minimum_required(VERSION 3.22)
project(bit-manipulation)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

set(WARNING_OPTIONS -Wall -Wimplicit-fallthrough -Wpedantic -Wextra -Werror -Wconversion -Wno-bit-int-extension)
set(SANITIZER_OPTIONS
#-fsanitize=address,undefined
)

set(HEADERS
    src/bmd/doc_to_html.hpp
    src/bmd/fwd.hpp
    src/bmd/parse.hpp
    src/bmd/html_writer.hpp
    src/bmd/scope_success_writer.hpp

    src/bms/analysis_error.hpp
    src/bms/analyze.hpp
    src/bms/ast.hpp
    src/bms/astp.hpp
    src/bms/concrete_type.hpp
    src/bms/concrete_value.hpp
    src/bms/deduction.hpp
    src/bms/execution_error.hpp
    src/bms/grammar.hpp
    src/bms/linear_map_stack.hpp
    src/bms/operations.hpp
    src/bms/parse.hpp
    src/bms/tokens.hpp
    src/bms/tokenize.hpp
    src/bms/value.hpp
    src/bms/vm_codegen.hpp
    src/bms/vm_instructions.hpp
    src/bms/vm.hpp

    src/common/ansi.hpp
    src/common/assert.hpp
    src/common/config.hpp
    src/common/fwd.hpp
    src/common/io.hpp
    src/common/parse.hpp
    src/common/source_position.hpp
    src/common/result.hpp
    src/common/visit.hpp

    src/cli/diagnostics.hpp)

add_executable(bmscript ${HEADERS}
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

    src/common/assert.cpp
    src/common/io.cpp
    src/common/parse.cpp

    src/cli/diagnostics.cpp
    src/cli/main.cpp)

target_compile_options(bmscript PRIVATE ${WARNING_OPTIONS} ${SANITIZER_OPTIONS})
target_link_options(bmscript PRIVATE ${SANITIZER_OPTIONS})
target_include_directories(bmscript PRIVATE "${CMAKE_CURRENT_LIST_DIR}")
target_include_directories(bmscript PRIVATE "${CMAKE_CURRENT_LIST_DIR}/src")

# add_executable(test ${HEADERS} )
