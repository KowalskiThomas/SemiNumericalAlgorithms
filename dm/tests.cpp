#include "code.h"
#include "utils.h"

auto test_power() -> void
{
  std::cout << "Tests power" << std::endl;
  for(entier a = 1; a < 1000; a++)
  {
    assert(power(entier(a), 0) == 1);
    assert(power(entier(a), 1) == a);
    assert(a * a * a * a * a == power<unsigned long>(entier(a), 5));
  }
  std::cout << "=> OK" << std::endl;
}

template <unsigned int n>
auto test_root() -> void
{
  const auto min = 0;
  const auto max = 999;
  std::cout << "Tests root<" << n << "> from " << min << " to " << max << std::endl;
  for(entier i = 1; i < max; i++)
  {
    auto root_i = root<n>(i);
    assert(root_i <= i);
    assert(root_i * root_i <= i);
    assert(power(root_i + 1, n) > i);
  }
  std::cout << "=> OK" << std::endl;
}

template <int n>
auto test_all_roots() -> void
{
  test_root<n>();
  test_all_roots<n - 1>();
}

template <>
auto test_all_roots<1>() -> void{

};

template <int n>
auto time_all_roots()
{
  std::cout << n << std::endl;
  test_time(roots_up_to<n>::f);
  test_all_roots<n - 1>();
}

template <>
auto time_all_roots<1>()
{
  std::cout << 1 << std::endl;
  test_time(roots_up_to<1>::f);
}

auto test_factor2(const entier max_iter = 1000, const bool display_info = false, const bool display_results = false) -> void
{
  const auto min = 1;
  const auto max = 999;
  std::cout << "Test factorization a = pq from " << min << " to " << max << std::endl;
  for(entier t = min; t < max; t++)
  {
    auto f = factor_power(t, t / 4);
    if(f.first != t && f.second != 1)
    {
      if(display_info)
        std::cout << "Can't test factor_pq with factorizable '" << t << "'" << std::endl;
      continue;
    }
    else if(is_prime(t))
    {
      if(display_info)
        std::cout << "Can't test factor_pq with prime '" << t << "'" << std::endl;
      continue;
    }

    auto p_q = factor2(t, max_iter);

    if(!p_q)
    {
      if(display_info)
        std::cout << "Couldn't find (p, q) for '" << t << "'" << std::endl;
      continue;
    }
    auto p = p_q->first;
    auto q = p_q->second;

    if(display_info || display_results)
      std::cout << "For " << t << " p = " << p << ", q = " << q << std::endl;

    assert(p * q == t);
    assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == t);
  }
  std::cout << "=> OK" << std::endl;
}

auto test_factor3(const entier max_iter = 1000, const bool display_info = false, const bool display_results = false) -> void
{
  const auto min = 1;
  const auto max = 999;
  std::cout << "Test factorization a = pq with a = a³-b³ from " << min << " to " << max << std::endl;
  for(entier t = min; t < max; t++)
  {
    auto opq = factor3(t, t);
    if(!opq)
    {
      if(display_info)
        std::cout << "Couldn't find factorisation for " << t << std::endl;
      continue;
    }

    auto pq = *opq;
    auto p = pq.first;
    auto q = pq.second;

    if(p == 1)
    {
      if(display_info)
        std::cout << "Factorization for " << t << " is 1 and " << q << std::endl;
      continue;
    }

    if(display_results)
      std::cout << p << " * " << q << " = " << t << std::endl;
    assert(p * q == t);
  }
  std::cout << "=> OK" << std::endl;
}

roots_up_to<1500> _roots_up_to;
auto test_roots() -> void
{
  test_all_roots<50>();
}

auto test_factor_power(const entier max_iter = 10000) -> void
{
  entier x = 125;
  auto result = factor_power(x, max_iter);

  std::cout << "Best factor for " << x << " : " << result.first << " / " << result.second << std::endl;
}

auto main() -> int
{
  test_power();
  test_factor_power();
  test_factor2();
  test_factor3();
  test_roots();
  return 0;
}
