#include <iostream>
#include <gmpxx.h>

using entier = mpz_class;

size_t get_size(const entier x)
{
    return std::abs(x.get_mpz_t()->_mp_size);
}

auto exo1(const entier a) -> entier
{
    /**
     * D'après gnuplot, c'est un algorithme de type assez quadratique.
     * gnuplot> plot "data.txt" 1:($2/$1/$1) -> constant
     */
    if (a == 0)
        return 0;

    auto r = entier(a);
    entier r_before = r + 1;

    while (r < r_before)
    {
        r_before = r;
        r = (r + (a / r)) / 2;
    }

    return r_before;
}

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
        std::cout << "sqrt(" << a << ") = " << exo1(a) << std::endl;
    }
}

#include <algorithm>
#include <functional>

auto test_time() -> void
{
    for (int i = 0; i < 10000; i++)
    {
        auto a = get_random(i);
        auto f = std::bind(exo1, a);
        auto t = get_time(f);
        std::cout << i;
        // std::cout << " " << a;
        std::cout << " " << t << std::endl;
    }
}

auto main() -> int
{
    test_time();
    test_exo1();
    return 0;
}