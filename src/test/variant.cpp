#include <gtest/gtest.h>

#include "common/variant.hpp"

namespace bit_manipulation {
namespace {

TEST(Variant, trivial_basics)
{
    using V = Variant<int, float>;

    static_assert(std::is_trivially_copy_constructible_v<V>);
    static_assert(std::is_trivially_move_constructible_v<V>);
    static_assert(std::is_trivially_copyable_v<V>);
    static_assert(has_alternative_v<V, int>);
    static_assert(has_alternative_v<V, float>);

    V v;
    EXPECT_TRUE(holds_alternative<int>(v));

    v = 0;
    EXPECT_TRUE(holds_alternative<int>(v));
    v = 0.f;
    EXPECT_TRUE(holds_alternative<float>(v));
}

TEST(Variant, trivial_special_members)
{
    Variant<int, float> a;
    EXPECT_TRUE(holds_alternative<int>(a));

    Variant<int, float> b = 0;
    EXPECT_TRUE(holds_alternative<int>(b));

    Variant<int, float> c = 0.f;
    EXPECT_TRUE(holds_alternative<float>(c));

    Variant<int, float> d = a;
    EXPECT_TRUE(d.index() == a.index());

    Variant<int, float> e = std::move(b);
    EXPECT_TRUE(e.index() == b.index());

    d = e;
    EXPECT_TRUE(d.index() == e.index());

    e = std::move(c);
    EXPECT_TRUE(e.index() == c.index());
}

struct Non_Default_Constructible {
    Non_Default_Constructible() = delete;
    Non_Default_Constructible(int) { }
};

TEST(Variant, non_default_constructible)
{
    static_assert(!std::is_default_constructible_v<Variant<Non_Default_Constructible>>);
    static_assert(!std::is_default_constructible_v<Variant<Non_Default_Constructible, int>>);
    static_assert(std::is_default_constructible_v<Variant<int, Non_Default_Constructible>>);

    Variant<int, Non_Default_Constructible> a;
    EXPECT_TRUE(holds_alternative<int>(a));

    Variant<int, Non_Default_Constructible> b = 0;
    EXPECT_TRUE(holds_alternative<int>(b));
}

struct Counted {
    int* m_counter;

    Counted(int& counter)
        : m_counter(&counter)
    {
        ++*m_counter;
    }

    Counted(const Counted& c) noexcept
        : m_counter(c.m_counter)
    {
        ++*m_counter;
    }

    Counted(Counted&& c) noexcept
        : m_counter(c.m_counter)
    {
        ++*m_counter;
    }

    Counted& operator=(const Counted&) = default;
    Counted& operator=(Counted&&) = default;

    ~Counted()
    {
        --*m_counter;
    }
};

TEST(Variant, manages_lifetimes)
{
    int counter = 0;

    {
        Variant<int, Counted> a;
        EXPECT_EQ(counter, 0);

        a = Counted(counter);

        EXPECT_TRUE(holds_alternative<Counted>(a));
        EXPECT_EQ(counter, 1);

        {
            Variant<int, Counted> b = a;
            EXPECT_TRUE(holds_alternative<Counted>(b));
            EXPECT_EQ(counter, 2);

            {
                Variant<int, Counted> c = std::move(a);
                EXPECT_TRUE(holds_alternative<Counted>(c));
                EXPECT_EQ(counter, 3);

                a = c;
                EXPECT_TRUE(holds_alternative<Counted>(a));
                EXPECT_EQ(counter, 3);

                Variant<int, Counted> d = a;
                EXPECT_TRUE(holds_alternative<Counted>(d));
                EXPECT_EQ(counter, 4);

                d = 0;
                EXPECT_TRUE(holds_alternative<int>(d));
                EXPECT_EQ(counter, 3);
            }

            EXPECT_EQ(counter, 2);
        }
        EXPECT_EQ(counter, 1);
    }

    EXPECT_EQ(counter, 0);
}

} // namespace
} // namespace bit_manipulation
