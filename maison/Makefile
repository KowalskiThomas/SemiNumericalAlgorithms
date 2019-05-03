all:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml karatsuba.ml -o karatsuba
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml tests.ml -o tests

run: all
	./karatsuba

tests: all
	./tests

clean:
	rm -rf _build *.cmi *.cmo *.byte *.native *.cmx

tar: clean
	tar cvf CalML.tar.gz *
