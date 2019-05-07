#include <iostream>
#include <gmpxx.h>

using entier = mpz_class;

const long N = 10000

auto methode1(entier a, entier b, entier c, entier m) -> entier
{
	auto res = a * b * c;
	res = res % m;
	return res;
}

auto methode2(entier a, entier b, entier c, entier m) -> entier
{
	auto res = a * b;
	res = res % m;
	res = res * c;
	res = res % m;
	return res;
}

auto main() -> int
{
	auto f = methode1;

	gmp_randclass rand(gmp_init_default);
	for(int i = 0; i < N; i++)
	{
		auto a = rand.get_z_bits(64 * i);
		auto b = rand.get_z_bits(64 * i);
		auto c = rand.get_z_bits(64 * i);
		auto m = rand.get_z_bits(64 * i);
		
		size_t tick = clock();
		auto res = f(a, b, c, m);
		size_t tock = clock();
		auto time = tock - tick;
		
		std::cout << a << b << c << m << time << std::endl;
		
	}	
	return 0;
}
