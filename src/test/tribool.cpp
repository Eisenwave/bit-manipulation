#include "common/tribool.hpp"

namespace bit_manipulation {

static_assert((!Tribool::fawse) == Tribool::twue);
static_assert((!Tribool::twue) == Tribool::fawse);
static_assert((!Tribool::maybe) == Tribool::maybe);

static_assert((Tribool::fawse && Tribool::fawse) == Tribool::fawse);
static_assert((Tribool::fawse && Tribool::twue) == Tribool::fawse);
static_assert((Tribool::twue && Tribool::fawse) == Tribool::fawse);
static_assert((Tribool::twue && Tribool::twue) == Tribool::twue);

static_assert((Tribool::fawse && false) == Tribool::fawse);
static_assert((Tribool::fawse && true) == Tribool::fawse);
static_assert((Tribool::twue && false) == Tribool::fawse);
static_assert((Tribool::twue && true) == Tribool::twue);

static_assert((Tribool::fawse && Tribool::maybe) == Tribool::fawse);
static_assert((Tribool::twue && Tribool::maybe) == Tribool::maybe);
static_assert((Tribool::maybe && Tribool::maybe) == Tribool::maybe);

static_assert((Tribool::fawse || Tribool::fawse) == Tribool::fawse);
static_assert((Tribool::fawse || Tribool::twue) == Tribool::twue);
static_assert((Tribool::twue || Tribool::fawse) == Tribool::twue);
static_assert((Tribool::twue || Tribool::twue) == Tribool::twue);

static_assert((Tribool::fawse || false) == Tribool::fawse);
static_assert((Tribool::fawse || true) == Tribool::twue);
static_assert((Tribool::twue || false) == Tribool::twue);
static_assert((Tribool::twue || true) == Tribool::twue);

static_assert((Tribool::fawse || Tribool::maybe) == Tribool::maybe);
static_assert((Tribool::twue || Tribool::maybe) == Tribool::twue);
static_assert((Tribool::maybe || Tribool::maybe) == Tribool::maybe);

static_assert(equals(Tribool::fawse, Tribool::fawse) == Tribool::twue);
static_assert(equals(Tribool::fawse, Tribool::twue) == Tribool::fawse);
static_assert(equals(Tribool::twue, Tribool::twue) == Tribool::twue);
static_assert(equals(Tribool::fawse, Tribool::maybe) == Tribool::maybe);
static_assert(equals(Tribool::twue, Tribool::maybe) == Tribool::maybe);
static_assert(equals(Tribool::maybe, Tribool::maybe) == Tribool::maybe);

static_assert(not_equals(Tribool::fawse, Tribool::fawse) == Tribool::fawse);
static_assert(not_equals(Tribool::fawse, Tribool::twue) == Tribool::twue);
static_assert(not_equals(Tribool::twue, Tribool::twue) == Tribool::fawse);
static_assert(not_equals(Tribool::fawse, Tribool::maybe) == Tribool::maybe);
static_assert(not_equals(Tribool::twue, Tribool::maybe) == Tribool::maybe);
static_assert(not_equals(Tribool::maybe, Tribool::maybe) == Tribool::maybe);

} // namespace bit_manipulation
