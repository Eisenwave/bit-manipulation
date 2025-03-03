#ifndef BIT_MANIPULATION_BASIC_DIAGNOSTIC_CONSUMER_HPP
#define BIT_MANIPULATION_BASIC_DIAGNOSTIC_CONSUMER_HPP

#include <vector>

#include "bms/analysis_error.hpp"
#include "bms/diagnostic_consumer.hpp"
#include "bms/parsing/parse_error.hpp"
#include "bms/tokenization/tokenize_error.hpp"

namespace bit_manipulation::bms {

struct Basic_Diagnostic_Consumer final : virtual Diagnostic_Consumer {

    std::vector<Tokenize_Error> tokenize_errors;
    std::vector<Parse_Error> parse_errors;
    std::vector<Analysis_Error> analysis_errors;

    bms::Error_Reaction m_reaction;

    explicit Basic_Diagnostic_Consumer(bms::Error_Reaction reaction)
        : m_reaction { reaction }
    {
    }

    bms::Error_Reaction operator()(Tokenize_Error&& error) override
    {
        tokenize_errors.push_back(std::move(error));
        return m_reaction;
    }

    bms::Error_Reaction operator()(Parse_Error&& error) override
    {
        parse_errors.push_back(std::move(error));
        return m_reaction;
    }

    bms::Error_Reaction operator()(Analysis_Error&& error) override
    {
        analysis_errors.push_back(std::move(error));
        return m_reaction;
    }

    Size error_count() const noexcept override
    {
        return tokenize_errors.size() + parse_errors.size() + analysis_errors.size();
    }

    void clear() noexcept override
    {
        tokenize_errors.clear();
        parse_errors.clear();
        analysis_errors.clear();
    }
};

} // namespace bit_manipulation::bms

#endif
