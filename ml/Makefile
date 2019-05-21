all: clean tests perf

perf:
	ocamlc nums.cma Degres.ml Coeffs.ml Polynomes.ml Generator.ml tests_perf.ml -o perf -g

tests: tests_zpz tests_nums perf
	./tests_zpz
	./tests_nums
	./perf

tests_zpz:
	ocamlc nums.cma Degres.ml Coeffs.ml Polynomes.ml Generator.ml tests_zpz.ml -o tests_zpz -g

tests_perf_complet:
	ocamlc nums.cma Degres.ml Coeffs.ml Polynomes.ml Generator.ml tests_perfs_complet.ml -o tests_perfs_complet -g

tests_nums:
	ocamlc nums.cma Degres.ml Coeffs.ml Polynomes.ml Generator.ml tests_nums.ml -o tests_nums -g

clean:
	rm -rf test *.cmi *.o *.cmo *.cmx a.out tests_zpz tests_nums perf

tar: clean
	tar cvzf ASN-ThomasKowalski.tgz *.ml *.pdf 