#ifndef UTILS
#define UTILS

#include <experimental/optional>
// #include <optional>
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <ctime>
#include <functional>
#include <gmpxx.h>
#include <iostream>

using std::size_t;
using std::experimental::nullopt;
using std::experimental::optional;
using entier = mpz_class;

struct couple_t
{
  entier a_;
  unsigned long b_;

  couple_t(entier a, unsigned long b)
      : a_(a), b_(b)
  {
  }
};

// using couple = std::pair<entier, unsigned long>;
// auto make_couple = std::make_pair<entier, unsigned long>;
using couple = couple_t;

size_t get_size(const entier x)
{
  return std::abs(x.get_mpz_t()->_mp_size);
}

template <typename Function>
auto get_exec_time(const Function f) -> clock_t
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

template <typename Int>
auto power(entier a, Int b) -> entier
{
  assert(b >= 0);

  if(b == 0)
    return 1;

  auto res = entier();
  mpz_pow_ui(res.get_mpz_t(), a.get_mpz_t(), b);
  return res;
}

auto _1 = power<unsigned long>;
auto _2 = power<int>;
auto _3 = power<unsigned long long>;
auto _4 = power<unsigned int>;

auto is_prime(entier a) -> bool
{
  if(a == 1 || a == 2)
    return true;
  for(auto i = 2; i < a; i++)
    if(a % i == 0)
      return false;

  return true;
}

#endif