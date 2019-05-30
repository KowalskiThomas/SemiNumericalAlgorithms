#include<gmpxx.h>

using entier = mpz_class;
using std::size_t;

typedef struct couple {
		unsigned long int k;
		entier r;
} couple;

unsigned long int log2(entier r);
couple one_factor(entier a);
entier power(entier a, int k);
size_t get_size(const entier x);
entier sqrt_v2(entier a);
entier kroot_v2(entier a, int k);




