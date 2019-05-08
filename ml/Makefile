nums:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml tests_nums.ml -o tests_nums
	./tests_nums

zpz:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml tests_zpz.ml -o tests_zpz
	./tests_zpz

generator:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml generateur.ml -o generator
	./generator

run: nums zpz

clean:
	rm -rf _build *.cmi *.cmo *.byte *.native *.cmx sandbox karatsuba tests_zpz generator tests_nums

tar: clean
	tar cvf CalML.tar.gz *
