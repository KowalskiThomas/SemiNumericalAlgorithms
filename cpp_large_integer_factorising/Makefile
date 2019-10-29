all: tests perf

one_factor_test : kroot.o one_factor_test.cpp
	g++ kroot.o one_factor_test.cpp -lgmp -lgmpxx -o one_factor_test -g

kroot_test : kroot.o yeti_test_kroot.cpp
	g++ kroot.o yeti_test_kroot.cpp -lgmp -lgmpxx -o kroot_test -g

yeti_test : kroot.o yeti_test.cpp
	g++ kroot.o yeti_test.cpp -lgmp -lgmpxx -o yeti_test -g

kroot.o : kroot.cpp kroot.h
	g++ -c kroot.cpp -lgmp -lgmpxx 

tests:
	g++ tests.cpp -lgmp -lgmpxx -o tests -std=c++17 -O2
	
perf:
	g++ perftest.cpp -lgmp -lgmpxx -o perf -std=c++17 -O2

clean:
	@echo -n "Nettoyage... "
	@rm -rf exo a.out  tests perf
	@echo OK

format: clean
	@echo -n "Formattage... "
	@clang-format -i *.h *.cpp 
	@echo "OK"

cleandata:
	rm -rf *.txt
