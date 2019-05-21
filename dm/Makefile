all: tests perf

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
