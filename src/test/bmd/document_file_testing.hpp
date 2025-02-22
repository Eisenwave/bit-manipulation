#ifndef BIT_MANIPULATION_TEST_DOCUMENT_FILE_TESTING_HPP
#define BIT_MANIPULATION_TEST_DOCUMENT_FILE_TESTING_HPP

#include <string_view>

#include "test/compilation_stage.hpp"

namespace bit_manipulation {

bool test_for_success(std::string_view file, BMD_Stage until_stage = BMD_Stage::parse);

}

#endif
