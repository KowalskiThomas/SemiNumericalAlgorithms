all:
	g++ tests.cpp -lgmp -lgmpxx -o tests -g -O3 -std=c++17
	
clean:
	rm -rf exo a.out

format:
	clang-format -i *.h *.cpp

cleandata:
	rm -rf *.txt
