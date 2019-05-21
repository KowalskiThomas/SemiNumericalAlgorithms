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

using std::pair;
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

/* Renvoie le temps d'exécution d'une fonction d'arité nulle. */
template <typename Function>
auto get_exec_time(const Function f) -> clock_t
{
  auto tick = clock();
  f();
  auto tock = clock();
  return tock - tick;
}

/* Renvoie un grand entier aléatoire sur n limbs */
auto get_random(const unsigned int limbs) -> entier
{
  static gmp_randclass rand(gmp_randinit_default);
  return rand.get_z_bits(limbs * sizeof(mp_limb_t));
}

/* Fonction puissance */
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

/* Vérifie si un entier est premier */
auto is_prime(entier a) -> bool
{
  if(a == 1 || a == 2)
    return true;
  for(auto i = 2; i < a; i++)
    if(a % i == 0)
      return false;

  return true;
}

/* Teste le temps d'exécution d'une fonction de 1 à 1000 limbs */
template <typename Function>
auto test_time(Function f, unsigned int min_limbs = 1, unsigned int max_limbs = 1000) -> void
{
  for(int i = min_limbs; i < max_limbs; i++)
  {
    // On prend un grand entier sur i limbs
    auto a = get_random(i);
    auto f_bound = std::bind(f, a);
    auto t = get_exec_time(f_bound);
    std::cout << i;
    std::cout << " " << t << std::endl;
  }
}

#endif