#include<iostream>
#include<ctime>
#include"kroot.h"

int main(void)
{
		gmp_randclass rstate (gmp_randinit_default);


		for (int i = 2; i < 100; i++)
		{
				entier a = rstate.get_z_bits(2*i);
				std::clock_t t = std::clock();
				couple b = one_factor(a);
				t = std::clock() - t;
				std::cout << 10*i << " " <<
						(float) (1000*t)/CLOCKS_PER_SEC
						<< std::endl;
		}

		return 1;
}
