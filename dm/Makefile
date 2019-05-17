all:
	g++ tests.cpp -lgmp -lgmpxx -o tests -g -O3 -std=c++17
	
clean:
	@echo -n "Nettoyage... "
	@rm -rf exo a.out
	@echo OK

format: clean
	@echo -n "Formattage... "
	@clang-format -i *.h *.cpp
	@echo "OK"

cleandata:
	rm -rf *.txt
