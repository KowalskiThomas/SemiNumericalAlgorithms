#include <time.h>
#include <stdio.h>
#include <gmpxx.h>
#include <iostream>
#include <memory>
#include <algorithm>
#include <ctime>

using mpz = mpz_class;

template<typename Function>
double measure_time(const Function& f)
{
	struct timespec t0, t1;
	clock_gettime(CLOCK_REALTIME, &t0);
	f();
	clock_gettime(CLOCK_REALTIME, &t1);
	return 1000.*(t1.tv_sec-t0.tv_sec) + (t1.tv_nsec-t0.tv_nsec)/1000000.;
}

mpz_class multiply(const mpz a, const mpz b)
{
	return a * b;
}

int main()
{
	mpz_class a;
	mpz_class b;

	gmp_randclass rand(gmp_randinit_default);
	rand.seed(time(NULL));

	a = rand.get_z_bits(1289900 * 8 * sizeof(mp_limb_t));
	b = rand.get_z_bits(1289900 * 8 * sizeof(mp_limb_t));

	// std::cout << a << " * " << b << " = " << a * b << std::endl;

	auto to_execute = std::bind(multiply, a, b);
	
	auto result = measure_time(to_execute);
	std::cout << result << " us" << std::endl;

	return 0;
}	
