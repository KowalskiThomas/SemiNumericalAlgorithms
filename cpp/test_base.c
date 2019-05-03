#include <time.h>
#include <stdio.h>
#include <gmp.h>

int main()
{
	mpz_t a;
	mpz_t b;
	mpz_t res;

	mpz_init(a);
	mpz_init(b);
	mpz_init(res);

	gmp_randstate_t rand;
	gmp_randinit_default(rand);
	gmp_randseed_ui(rand, time(NULL));

	mpz_urandomb(a, rand, 5 * 8 * sizeof(mp_limb_t));
	mpz_urandomb(b, rand, 5 * 8 * sizeof(mp_limb_t));

	char s[255];
	mpz_get_str((char*)&s, 10, a);
	printf("a = %s\n", s);
	mpz_get_str((char*)&s, 10, b);
	printf("b = %s\n", s);

	mpz_mul(res, b, a);
	mpz_get_str((char*)&s, 10, res);
	printf("res = %s\n", s);


	gmp_randclear(rand);
	mpz_clear(a);
	mpz_clear(b);
	mpz_clear(res);

	return 0;
}	
