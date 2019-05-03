test:
	gcc test_base.c -lgmp
testcpp:
	g++ test_cpp.cpp -lgmp -lgmpxx
karatsuba:
	g++ karatsuba.cpp -lgmp -lgmpxx -O3
plot:
	plot results.txt using 2:($3/($2**(log(3)/log(2))))