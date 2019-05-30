#include<iostream>
#include<ctime>
#include"kroot.h"

int main(void)
{
		gmp_randclass rstate (gmp_randinit_default);

		entier a = entier(1) << 512;

		entier b = kroot_v2(a, 512);

		std::cout << b << std::endl;
		/*entier a = rstate.get_z_bits(64*500000);


		for (int i = 2; i < 1000; i++)
		{
				std::clock_t t = std::clock();
				entier b = kroot_v2(a,i);
				t = std::clock() - t;
				std::cout << i << " " <<
						(float) (1000*t)/CLOCKS_PER_SEC
						<< std::endl;
		}*/

		return 1;
}
