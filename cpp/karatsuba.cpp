#include <time.h>
#include <stdio.h>
#include <gmpxx.h>
#include <iostream>
#include <memory>
#include <algorithm>
#include <ctime>

using mpz = mpz_class;
#define beta 1 << 64;

size_t get_size(const mpz x)
{
	return std::abs(x.get_mpz_t()->_mp_size);
}

mpz lower_part(const mpz a, const unsigned long n)
{
	mpz r;
	mpz_fdiv_r_2exp(r.get_mpz_t(), a.get_mpz_t(), n * 64);
	return r;	
}

mpz upper_part(const mpz a, const unsigned long n)
{
	mpz r;
	mpz_fdiv_q_2exp(r.get_mpz_t(), a.get_mpz_t(), n * 64);
	return r;
}

#include <cassert>

mpz karatsuba(const mpz a, const mpz b)
{
	const size_t ta = get_size(a);
	const size_t tb = get_size(b);

	assert (ta >= 0);
	assert (tb >= 0);

	if (ta <= 1 || tb <= 1)
		return a * b;

	const size_t moy = std::max(ta / 2, tb / 2);

	const mpz a1 = upper_part(a, moy);
	const mpz a0 = lower_part(a, moy);
	const mpz b1 = upper_part(b, moy);
	const mpz b0 = lower_part(b, moy);

	const mpz c1 = karatsuba(a0 - a1, b0 - b1);
	const mpz c0 = karatsuba(a0, b0);
	const mpz cinf = karatsuba(a1, b1);

	return c0 + (cinf << (64 * moy * 2)) + ((cinf + c0 - c1) << (64 * moy));
}

template<typename Function>
double measure_time(const Function& f)
{
	struct timespec t0, t1;
	clock_gettime(CLOCK_REALTIME, &t0);
	f();
	clock_gettime(CLOCK_REALTIME, &t1);
	return 1000.*(t1.tv_sec-t0.tv_sec) + (t1.tv_nsec-t0.tv_nsec) / 1000000.;
}


int main()
{
	for (int i = 100; i < 99999; i++)
	{
		mpz a = 1;
		a = a << i;
		auto to_execute = std::bind(karatsuba, a, a);
		auto result = measure_time(to_execute);
		std::cout << i << ' ' << i << ' ' << result << std::endl;
	}

	return 0;
}