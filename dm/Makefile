all: tests perf

yeti_test : kroot.o utils.h yeti_test.cpp
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
