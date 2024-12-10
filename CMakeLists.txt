cmake_minimum_required(VERSION 3.22)
project(bit-manipulation)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT DEFINED EMSCRIPTEN)
    find_package(GTest REQUIRED)
    enable_testing()
endif()

set(LLVM_WARNING_OPTIONS -Wall -Wimplicit-fallthrough -Wpedantic -Wextra -Wconversion -Wmissing-declarations -Wno-bit-int-extension)

if(DEFINED EMSCRIPTEN)
    set(WARNING_OPTIONS ${LLVM_WARNING_OPTIONS})
    set(SANITIZER_OPTIONS)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(WARNING_OPTIONS -Wall -Wimplicit-fallthrough -Wpedantic -Wextra -Wconversion -Wmissing-declarations)
#    set(SANITIZER_OPTIONS -fsanitize=address,undefined)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set(WARNING_OPTIONS ${LLVM_WARNING_OPTIONS})
#    set(SANITIZER_OPTIONS -fsanitize=address,undefined)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(WARNING_OPTIONS -W4)
    set(SANITIZER_OPTIONS -fsanitize:address)
endif()

set(INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}" "${CMAKE_CURRENT_LIST_DIR}/src")
include_directories(${INCLUDE_DIRS})

set(HEADERS
    src/bmd/directive_type.hpp
    src/bmd/fwd.hpp
    src/bmd/scope_success_writer.hpp

    src/bmd/codegen/code_language.hpp
    src/bmd/codegen/code_span_type.hpp
    src/bmd/codegen/code_string.hpp
    src/bmd/codegen/codegen.hpp
    src/bmd/codegen/generator_base.hpp
    src/bmd/codegen/generator_error.hpp

    src/bmd/html/bms_to_html.hpp
    src/bmd/html/doc_to_html.hpp
    src/bmd/html/html_writer.hpp
    
    src/bmd/parsing/ast.hpp
    src/bmd/parsing/grammar.hpp
    src/bmd/parsing/parse.hpp

    src/bms/analysis_error.hpp
    src/bms/analysis_level.hpp
    src/bms/analyze.hpp
    src/bms/analyzed_program.hpp
    src/bms/ast.hpp
    src/bms/basic_diagnostic_consumer.hpp
    src/bms/comparison_failure.hpp
    src/bms/concrete_type.hpp
    src/bms/concrete_value.hpp
    src/bms/deduction.hpp
    src/bms/diagnostic_consumer.hpp
    src/bms/instantiate.hpp    
    src/bms/symbol_table.hpp
    src/bms/value.hpp

    src/bms/evaluation/builtin_function.hpp
    src/bms/evaluation/evaluation_error.hpp
    src/bms/evaluation/operations.hpp

    src/bms/parsing/astp.hpp
    src/bms/parsing/grammar.hpp
    src/bms/parsing/parse_error.hpp
    src/bms/parsing/parse.hpp
    
    src/bms/tokenization/token_type.hpp
    src/bms/tokenization/tokenize_error.hpp
    src/bms/tokenization/tokenize.hpp
    src/bms/tokenization/token.hpp

    src/bms/vm/execution_error.hpp
    src/bms/vm/linear_map_stack.hpp
    src/bms/vm/codegen.hpp
    src/bms/vm/instructions.hpp
    src/bms/vm/vm.hpp

    src/common/ansi.hpp
    src/common/assert.hpp
    src/common/config.hpp
    src/common/diagnostics.hpp
    src/common/fwd.hpp
    src/common/io.hpp
    src/common/io_error.hpp
    src/common/packs.hpp
    src/common/parse.hpp
    src/common/result.hpp
    src/common/source_position.hpp
    src/common/to_string.hpp
    src/common/tribool.hpp
    src/common/tty.hpp
    src/common/variant.hpp
)

add_library(bm STATIC ${HEADERS}
    src/bmd/enums.cpp
    src/bmd/codegen/codegen.cpp
    src/bmd/codegen/codegen_bms.cpp
    src/bmd/codegen/codegen_c_cpp.cpp
    src/bmd/html/bms_to_html.cpp
    src/bmd/html/doc_to_html.cpp
    src/bmd/html/html_writer.cpp
    src/bmd/parsing/parse.cpp

    src/bms/analyze_annotations.cpp
    src/bms/analyze_name_lookup.cpp
    src/bms/analyze.cpp
    src/bms/ast.cpp
    src/bms/instantiate.cpp
    src/bms/util.cpp
    src/bms/evaluation/operations.cpp
    src/bms/parsing/parse.cpp
    src/bms/tokenization/tokenize.cpp
    src/bms/vm/codegen.cpp
    src/bms/vm/instructions.cpp
    src/bms/vm/vm.cpp

    src/common/diagnostics.cpp
    src/common/io.cpp
    src/common/parse.cpp
    src/common/tty.cpp
)

target_compile_options(bm PUBLIC ${WARNING_OPTIONS} ${SANITIZER_OPTIONS})
target_link_options(bm PUBLIC ${SANITIZER_OPTIONS})

if(DEFINED EMSCRIPTEN)
    # https://stunlock.gg/posts/emscripten_with_cmake/
    add_executable(bm-wasm ${HEADERS}
        src/wasm/bm.cpp
        src/wasm/bm.hpp
    )
    target_link_libraries(bm-wasm bm)
    set_target_properties(bm-wasm PROPERTIES
        OUTPUT_NAME "bm"
        SUFFIX ".wasm"
    )
    target_compile_options(bm-wasm PRIVATE
        # the SHELL option group prevents de-duplication
        # https://cmake.org/cmake/help/latest/prop_tgt/COMPILE_OPTIONS.html#option-de-duplication
        "SHELL:-s SIDE_MODULE=0"
        "-stdlib=libc++"
        "-Os"
    )
    target_link_options(bm-wasm PRIVATE
        "-Os"
        "-static-libc++"
        "-stdlib=libc++"
        "SHELL:-s WASM=1"
        "SHELL:-s ENVIRONMENT=web"
        "SHELL:-s AUTO_JS_LIBRARIES=0"
        "SHELL:-s SIDE_MODULE=0"
        "SHELL:-s STANDALONE_WASM=1"
        "SHELL:-s \"EXPORTED_FUNCTIONS=['_bm_foreign_alloc','_bm_foreign_free','_bm_plus','_bm_length_as_string','_bm_length_as_string_result']\""
        "SHELL:-s \"EXPORTED_RUNTIME_METHODS=[]\""
        "--no-entry"
    )

else(NOT DEFINED EMSCRIPTEN)
    add_executable(bm-cli ${HEADERS}
        src/cli/compile.cpp
        src/cli/compile.hpp
        src/cli/glue.cpp
        src/cli/glue.hpp
        src/cli/main.cpp
    )

    add_executable(bm-test ${HEADERS}
        src/test/compilation_stage.hpp
        src/test/diagnostic_policy.hpp
        src/test/main.cpp
        src/test/program_file_testing.cpp
        src/test/tribool.cpp
        src/test/variant.cpp
    )

    target_link_libraries(bm-cli bm)
    target_link_libraries(bm-test bm GTest::GTest GTest::Main)
    add_test(NAME MyTest COMMAND bm-test)
endif()
