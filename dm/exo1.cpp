#include <algorithm>
#include <functional>
#include <cassert>
#include <iostream>
#include <gmpxx.h>

using entier = mpz_class;

size_t get_size(const entier x)
{
    return std::abs(x.get_mpz_t()->_mp_size);
}

template <typename Int>
auto power(entier a, Int b) -> entier
{
    assert(b >= 0);

    if (b == 0)
        return 1;

    auto res = entier();
    mpz_pow_ui(res.get_mpz_t(), a.get_mpz_t(), b);
    return res;
}

template <int N>
auto root(const entier a) -> entier
{
    static_assert(N > 0, "Cannot compute negative or null roots.");

    /**
     * D'après gnuplot, c'est un algorithme de type assez quadratique.
     * (en tout cas pour n = 2)
     * gnuplot> plot "data.txt" 1:($2/$1/$1) -> constant
     */
    if (a == 0)
        return 0;

    auto r = entier(a);
    entier r_before = r + 1;

    while (r < r_before)
    {
        r_before = r;
        r = ((N - 1) * r + (a / power(r, (N - 1)))) / N;
    }

    return r_before;
}

auto root2(const entier a, const unsigned int N) -> entier
{
    /**
     * D'après gnuplot, c'est un algorithme de type assez quadratique.
     * (en tout cas pour n = 2)
     * gnuplot> plot "data.txt" 1:($2/$1/$1) -> constant
     */
    if (a == 0)
        return 0;

    auto r = entier(a);
    entier r_before = r + 1;

    while (r < r_before)
    {
        r_before = r;
        r = ((N - 1) * r + (a / power(r, (N - 1)))) / N;
    }

    return r_before;
}

template <int n>
struct square_n
{
    static constexpr auto f = root<n>;
    typedef square_n<n - 1> next;
};

template <typename Function>
auto get_time(const Function f) -> clock_t
{
    auto tick = clock();
    f();
    auto tock = clock();
    return tock - tick;
}

auto get_random(const int n) -> entier
{
    static gmp_randclass rand(gmp_randinit_default);
    return rand.get_z_bits(n);
}

auto test_exo1() -> void
{
    int ints[] = {10, 48, 49, 100, 1000};
    for (auto i = 0; i < sizeof(ints) / sizeof(int); i++)
    {
        auto a = ints[i];
        std::cout << "sqrt(" << a << ") = " << root<2>(a) << std::endl;
    }
}

template <typename Function>
auto test_time(Function f) -> void
{
    for (int i = 100; i < 1000; i++)
    {
        auto a = get_random(i);
        auto f_bound = std::bind(f, a);
        auto t = get_time(f_bound);
        std::cout << i;
        std::cout << " " << t << std::endl;
    }
}

template <int n>
auto test_all()
{
    std::cout << n << std::endl;
    test_time(square_n<n>::f);
    test_all<n - 1>();
}

template <>
auto test_all<1>()
{
    std::cout << 1 << std::endl;
    test_time(square_n<1>::f);
}

struct couple_t
{
    entier a_;
    unsigned long b_;

    couple_t(entier a, unsigned long b) : a_(a), b_(b) {}
};

// using couple = std::pair<entier, unsigned long>;
// auto make_couple = std::make_pair<entier, unsigned long>;
using couple = couple_t;

template <unsigned long k_max>
auto factor(entier a) -> couple
{
    auto best_base = entier();
    auto best_exp = 0;

    for (unsigned long exp = 1; exp < k_max; exp++)
    {
        auto expth_root = root2(a, exp);
        std::cout << exp << "V" << a << " = " << expth_root << std::endl;
        auto pow = power(expth_root, exp);
        std::cout << expth_root << "^" << exp << " = " << pow << std::endl;
        if (pow == a)
        {
            if (exp > best_exp)
            {
                best_exp = exp;
                best_base = expth_root;
            }
        }
    }
    // return make_couple(best_base, best_exp);
    return couple(best_base, best_exp);
}

auto factor_150 = factor<25>;

auto main() -> int
{
    entier x = 125;
    auto result = factor_150(x);
    std::cout << "Best factor for " << x << " : " << result.a_ << " / " << result.b_ << std::endl;
    // test_all<15>();
    return 0;
}