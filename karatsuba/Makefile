build_test_nums:
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml tests_nums.ml -o tests_nums
	
build_test_zpz:
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml tests_zpz.ml -o tests_zpz

build_test_generator:
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml generator.ml test_generator.ml -o tests_generator

tests: build_test_generator build_test_nums build_test_zpz
	./tests_nums
	./tests_zpz
	./tests_generator

generator:
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml generator.ml -o generator
	./generator

run: nums zpz

clean:
	rm -rf _build *.cmi *.cmo *.byte *.native *.cmx sandbox karatsuba tests_zpz generator tests_nums perf test_generator tests_generator a.out

merlin:
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml generator.ml

perf: 
	ocamlopt nums.cmxa Coeffs.ml Degs.ml polynome.ml generator.ml perftest_nums.ml -o perf

tar: clean
	tar cvf ASN-ThomasKowalski.tgz *
