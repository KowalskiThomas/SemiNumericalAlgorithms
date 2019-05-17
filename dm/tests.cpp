#include "code.h"
#include "utils.h"

auto test_power() -> void
{
  power<unsigned long>(entier(150), 5);
  power<unsigned long long>(entier(150), 5);
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

auto test_factor_pq(const bool display_info = false, const bool display_results = false) -> void
{
  const auto min = 1;
  const auto max = 999;
  std::cout << "Test factorization a = pq from " << min << " to " << max << std::endl;
  for(entier t = min; t < max; t++)
  {
    auto f = factor<150>(t);
    if(f.a_ != t && f.b_ != 1)
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

    auto p_q = find_p_q(t);

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

roots_up_to<1500> _roots_up_to;
auto test_roots() -> void
{
  test_all_roots<50>();
}

auto factorise = factor<150>;
auto test_factor() -> void
{
  entier x = 125;
  auto result = factorise(x);
  std::cout << "Best factor for " << x << " : " << result.a_ << " / " << result.b_ << std::endl;
}

auto main() -> int
{
  test_factor_pq();
  test_roots();
  test_factor();
  return 0;
}