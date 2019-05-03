all:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml karatsuba.ml -o karatsuba

yeti:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml yeti.ml sandbox.ml -o sandbox
	./sandbox

zpz:
	ocamlc -g nums.cma Coeffs.ml Degs.ml polynome.ml yeti.ml sandbox2.ml -o sandbox
	./sandbox

run: all
	./karatsuba

clean:
	rm -rf _build *.cmi *.cmo *.byte *.native *.cmx sandbox karatsuba

tar: clean
	tar cvf CalML.tar.gz *
