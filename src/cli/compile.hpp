#ifndef BIT_MANIPULATION_CLI_COMPILE_HPP
#define BIT_MANIPULATION_CLI_COMPILE_HPP

#include <memory_resource>
#include <string_view>
#include <vector>

#include "bms/parsing/parse.hpp"
#include "bms/tokenization/token.hpp"
#include "bms/tokenization/tokenize_error.hpp"

#include "bmd/parsing/parse.hpp"

namespace bit_manipulation {

std::pmr::vector<bms::Token> tokenize_bms_file(std::string_view source,
                                               std::string_view file,
                                               std::pmr::memory_resource* memory);

std::pmr::vector<char> load_file(std::string_view file, std::pmr::memory_resource* memory);

bms::Parsed_Program parse_tokenized(std::span<bms::Token const> tokens,
                                    std::string_view source,
                                    std::string_view file_name,
                                    std::pmr::memory_resource* memory);

bms::Analyzed_Program analyze_parsed(const bms::Parsed_Program& parsed,
                                     std::string_view file_name,
                                     std::pmr::memory_resource* memory);

} // namespace bit_manipulation

#endif
