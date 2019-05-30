#ifndef CODE
#define CODE

#include "utils.h"

/**
 * Racine N-ième
 */
template <int N>
auto root(const entier a) -> entier
{
  static_assert(N > 0, "Cannot compute negative or null roots.");

  if(a == 0)
    return 0;

  auto r = entier(a);
  entier r_before = r + 1;

  while(r < r_before)
  {
    r_before = r;
    r = ((N - 1) * r + (a / power(r, (N - 1)))) / N;
  }

  return r_before;
}

/**
 * Instancie toutes les root<k> pour k <= n
 */
template <int n>
struct roots_up_to
{
  static constexpr auto f = root<n>;
  typedef roots_up_to<n - 1> next;
};

/* Instanciation de root<1> à root<150> */
using _ = roots_up_to<150>;

/**
 * Racine N-ième mais sans template 
 */
auto root(const entier a, const unsigned int N) -> entier
{
  if(a == 0)
    return 0;

  auto r = entier(a);
  entier r_before = r + 1;

  while(r < r_before)
  {
    r_before = r;
    r = ((N - 1) * r + (a / power(r, (N - 1)))) / N;
  }

  return r_before;
}

/**
 * Factorise a = r^k (cherche à maximiser k)
 */
auto factor_power(entier a, entier k_max) -> pair<entier, entier>
{
  if(a == 0)
    return pair(0, 1);

  if(a == 1)
    return pair(1, 1);

  auto best_base = entier();
  auto best_exp = 0;

  for(unsigned long exp = 1; exp < k_max; exp++)
  {
    auto expth_root = root(a, exp);

    if(expth_root == 1)
      return pair(best_base, best_exp);

    auto pow = power(expth_root, exp);
    if(pow == a)
    {
      if(exp > best_exp)
      {
        best_exp = exp;
        best_base = expth_root;
      }
    }
  }
  return pair<entier, entier>(best_base, best_exp);
}

/**
 * Factorise a = x^2 - y^2
 */
auto factor2(const entier a, const entier max_iter) -> optional<std::pair<entier, entier>>
{
  assert(a != 1);

  if(a == 0)
    return std::pair<entier, entier>(0, 0);

  /* Normalement on devrait faire root(a) + 1 mais on le fait dans le do while */
  entier u = root<2>(a);

  auto is_square = false;
  entier iter = 0;
  entier diff;
  entier v;
  do
  {
    u = u + 1;
    diff = u * u - a;
    v = root<2>(diff);
    is_square = (v * v) == diff;
    iter++;
  } while(!is_square && iter < max_iter);

  if(iter >= max_iter)
    return nullopt;

  auto p = u + v;
  auto q = u - v;

  assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == a);

  return std::pair<entier, entier>(p, q);
}

/**
 * Factorise x = a^3 - b^3
 * max_a => Valeur maximale de a
 */
auto factor3(entier x, entier max_a) -> optional<pair<entier, entier>>
{
  entier a = 1;
  while(a < max_a)
  {
    const auto a3 = power(a, 3);
    const auto y = x + a3;
    const auto b = root<3>(y);
    const auto b3 = power(b, 3);
    if(b3 == y)
      return pair((b - a), (a * a + a * b + b * b));
    a++;
  }
  return nullopt;
}

#endif
