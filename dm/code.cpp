#include "code.h"
#include "utils.h"

template <int N>
auto root(const entier a) -> entier
{
  static_assert(N > 0, "Cannot compute negative or null roots.");

  /**
     * D'après gnuplot, c'est un algorithme de type assez quadratique.
     * (en tout cas pour n = 2)
     * gnuplot> plot "data.txt" 1:($2/$1/$1) -> constant
     */
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

using _ = square_n<150>;

auto root(const entier a, const unsigned int N) -> entier
{
  /**
     * D'après gnuplot, c'est un algorithme de type assez quadratique.
     * (en tout cas pour n = 2)
     * gnuplot> plot "data.txt" 1:($2/$1/$1) -> constant
     */
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

template <unsigned long k_max>
auto factor(entier a) -> couple
{
  if(a == 1)
    return couple(1, k_max);

  auto best_base = entier();
  auto best_exp = 0;

  for(unsigned long exp = 1; exp < k_max; exp++)
  {
    auto expth_root = root(a, exp);
    // std::cout << exp << "V" << a << " = " << expth_root << std::endl;
    auto pow = power(expth_root, exp);
    // std::cout << expth_root << "^" << exp << " = " << pow << std::endl;
    if(pow == a)
    {
      if(exp > best_exp)
      {
        best_exp = exp;
        best_base = expth_root;
      }
    }
  }
  // return make_couple(best_base, best_exp);
  return couple(best_base, best_exp);
}

auto __ = factor<150>;

template <unsigned long long max_iter>
auto find_p_q_(const entier a) -> optional<std::pair<entier, entier>>
{
  assert(a != 1);

  if(a == 0)
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
  } while(!is_square && iter < max_iter);

  if(iter >= max_iter)
    return nullopt;

  auto p = u + v;
  auto q = u - v;

  // auto y = u + v;
  // auto x = u - v;

  // auto p = (x + y) * (x + y) / 4;
  // auto q = (x - y) * (x - y) / 4;

  assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == a);

  return std::pair<entier, entier>(p, q);
}
