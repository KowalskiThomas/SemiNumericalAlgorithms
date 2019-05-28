#include<iostream>
#include"kroot.h"

int main(void)
{
		
		entier a = entier(3);
		entier c = power(a, 4);
		std::cout << c << std::endl;
		a = a << (64*10000);
		entier b = a * a;
		entier s = sqrt_v2(b);
		std::cout << get_size(b) <<" "<< get_size(s) << " " << (a == s) << std::endl;

		return 1;
}
