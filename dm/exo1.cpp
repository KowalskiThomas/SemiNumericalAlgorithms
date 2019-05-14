#include <optional>
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

auto test() -> void
{
    power<unsigned long>(entier(150), 5);
    power<unsigned long long>(entier(150), 5);
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
    std::cout << "Tests root<2>" << std::endl;
    for (entier i = 0; i < 99999; i++)
    {
        auto root_i = root<2>(i);
        assert(root_i <= i);
        assert(root_i * root_i <= i);
        assert((root_i + 1) * (root_i + 1) > i);
        // std::cout << "sqrt(" << i << ") = " << root<2>(a) << std::endl;
    }
    std::cout << "Tests OK" << std::endl;
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
    if (a == 1)
        return couple(1, k_max);

    auto best_base = entier();
    auto best_exp = 0;

    for (unsigned long exp = 1; exp < k_max; exp++)
    {
        auto expth_root = root2(a, exp);
        // std::cout << exp << "V" << a << " = " << expth_root << std::endl;
        auto pow = power(expth_root, exp);
        // std::cout << expth_root << "^" << exp << " = " << pow << std::endl;
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

template <unsigned long long max_iter>
auto find_p_q_(const entier a) -> std::optional<std::pair<entier, entier>>
{
    assert(a != 1);

    if (a == 0)
        return std::pair<entier, entier>(0, 0);

    /* Normalement on devrait faire root(a) + 1 mais on le fait dans le do while */
    entier u = root<2>(a);

    auto is_square = false;
    unsigned long long iter = 0;
    entier diff;
    entier v;
    do
    {
        u = u + 1;
        diff = u * u - a;
        v = root<2>(diff);
        is_square = (v * v) == diff;
        // std::cout << "u2=" << u * u << " delta = " << diff << " " << v * v << ' ' << is_square << std::endl;
        iter++;
    } while (!is_square && iter < max_iter);

    if (iter >= max_iter)
        return std::nullopt;

    auto p = u + v;
    auto q = u - v;

    // auto y = u + v;
    // auto x = u - v;

    // auto p = (x + y) * (x + y) / 4;
    // auto q = (x - y) * (x - y) / 4;

    assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == a);

    return std::pair<entier, entier>(p, q);
}
auto find_p_q = find_p_q_<1000>;

template <unsigned long long max>
auto is_prime_(entier a) -> bool
{
    if (a == 1 || a == 2)
        return true;
    for (auto i = 2; i < a; i++)
        if (a % i == 0)
            return false;

    return true;
}

auto is_prime = is_prime_<1000>;

auto test_factor_pq() -> void
{
    // auto tests = {2, 15, 16, 20, 32, 256};
    // auto tests = {1 ... 1000};
    // for (const auto &t : tests)
    for (entier t = 1; t < 999; t++)
    {
        auto f = factor<150>(t);
        if (f.a_ != t && f.b_ != 1)
        {
            std::cout << "Can't test factor_pq with factorizable '" << t << "'" << std::endl;
            continue;
        }
        else if (is_prime(t))
        {
            std::cout << "Can't test factor_pq with prime '" << t << "'" << std::endl;
            continue;
        }

        std::cout << "Test " << t << " (sqrt = " << root<2>(t) << ")" << std::endl;
        auto p_q = find_p_q(t);

        if (!p_q)
        {
            std::cout << "Couldn't find (p, q) for '" << t << "'" << std::endl;
            continue;
        }
        auto p = p_q->first;
        auto q = p_q->second;

        assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == t);
    }
}

auto factor_150 = factor<25>;

square_n<1500> _;

auto test_roots() -> void
{
    test_all<50>();
    entier x = 125;
    auto result = factor_150(x);
    std::cout << "Best factor for " << x << " : " << result.a_ << " / " << result.b_ << std::endl;
}

auto main() -> int
{
    test_factor_pq();
    // test_roots();
    return 0;
}