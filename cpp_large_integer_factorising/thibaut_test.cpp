#include<iostream>
#include<ctime>
#include"kroot.h"

int main(void)
{
		gmp_randclass rstate (gmp_randinit_default);
	
		for (int i = 1; i<10000; i++)
		{
				entier a = rstate.get_z_bits(64*(100*i + (i%2)));
				std::clock_t t = std::clock();
				a = sqrt_v2(a);
				t = std::clock() - t;
				std::cout << i*100 + (i%2)<< " " 
						<< (float)(1000*t)/CLOCKS_PER_SEC 
						<< std::endl;
		}
		
		return 1;
}
