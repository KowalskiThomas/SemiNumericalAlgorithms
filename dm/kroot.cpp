#include"kroot.h"

size_t get_size(const entier x)
{
  return std::abs(x.get_mpz_t()->_mp_size);
}

entier power(entier a, int k)
{
		if (k == 0) 
				return entier(1);
		else if (k == 1) 
				return entier(a);
		else if (k%2==0) 
				return power(a, k/2)* power(a,k/2);
		else 
				return power(a, k/2) * power(a,k/2) * a;
}

entier sqrt_v2(entier a)
{
		size_t n = get_size(a) ;
		entier r = entier(a);
		if (n >= 2) 
		{
				if (n % 2 == 1)
				{
						r = r >> ((n-1) * 64); 
						r = sqrt_v2(r) << (((n-1)) * 32);
				}
				else 
				{
						r = r >> ((n-2) * 64); 
						r = sqrt_v2(r) << (((n-2)) * 32);
				}
		}
		if (r == 0)
				return 0;

		entier r_before = entier(r) + 1;
		while (r < r_before)
		{
				r_before = r;
				r = ( r + (a / r) ) / 2;
		}
		return r_before;
}

entier kroot_v2(entier a, int k)
{
		size_t n = get_size(a);
		entier r = entier(a);
		if (n >= k)
		{
				int i = 1;
				while ((n-i)%k != 0)
				{
						i++;
				}
				r = r >> ((n-i) * 64);
				r = kroot_v2(r, k) << ((n-i)/k * 64);
		}

		if (r==0)
				return 0;

		entier r_before = entier(r)+1;
		while (r < r_before)
		{
				r_before = r;
				r = ( (k-1) * r + (a / power(r,(k-1)))) / k;
		}
		return r_before;
}


		
