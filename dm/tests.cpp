#include "code.h"
#include "utils.h"

auto test_power() -> void
{
  power<unsigned long>(entier(150), 5);
  power<unsigned long long>(entier(150), 5);
}

auto test_exo1() -> void
{
  std::cout << "Tests root<2>" << std::endl;
  for(entier i = 0; i < 99999; i++)
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
  for(int i = 100; i < 1000; i++)
  {
    auto a = get_random(i);
    auto f_bound = std::bind(f, a);
    auto t = get_exec_time(f_bound);
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

auto test_factor_pq() -> void
{
  for(entier t = 1; t < 999; t++)
  {
    auto f = factor<150>(t);
    if(f.a_ != t && f.b_ != 1)
    {
      std::cout << "Can't test factor_pq with factorizable '" << t << "'" << std::endl;
      continue;
    }
    else if(is_prime(t))
    {
      std::cout << "Can't test factor_pq with prime '" << t << "'" << std::endl;
      continue;
    }

    auto p_q = find_p_q(t);

    if(!p_q)
    {
      std::cout << "Couldn't find (p, q) for '" << t << "'" << std::endl;
      continue;
    }
    auto p = p_q->first;
    auto q = p_q->second;
    std::cout << "Pour " << t << " p = " << p << ", q = " << q << std::endl;

    assert(p * q == t);
    assert((p + q) * (p + q) / 4 - (p - q) * (p - q) / 4 == t);
  }
}

auto factor_150 = factor<25>;
square_n<1500> _square_n;

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