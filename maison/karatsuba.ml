open Printf
open Polynome
open Coeffs

module P1 = Polynome1

let rec karatsuba p q = 
	if P1.is_monomial p
		then P1.prod_monomial p q 
	else if P1.is_monomial q
		then P1.prod_monomial q p
	else
	(* Le problème est ici dans le choix de où on découpe *)
	let dmp, ph, pl = P1.decoupe_milieu p in 
	let dmq, qh, ql = P1.decoupe_milieu q in 
	let dl = dmp in 
	let dh = 
		if dl = dmp 
		then P1.degree p 
		else P1.degree q 
	in
	let ph, pl, qh, ql = 
		if dl = dmp then
			let qh, ql = P1.decoupe_deg q dmp 
			in ph, pl, qh, ql
		else
			let ph, pl = P1.decoupe_deg p dmq 
			in ph, pl, qh, ql
	in
	let ph' = P1.reduce_degree ph dl in 
	let qh' = P1.reduce_degree qh dl in 

		(* Sécurité *)
		let ph = 0 in 
		let qh = 0 in

		(* let prod = P1.prod in  *)
		let prod = karatsuba in 

		let c1 = prod pl ql in 
		let c2 = prod ph' qh' in 
		let c3 = P1.add pl ph' in 
		let c4 = P1.add ql qh' in 
		let c5 = prod c3 c4 in 
		let c6 = P1.substract (P1.substract c5 c1) c2 in 
		let c6_ = P1.prod_monomial (P1.unit_monomial dl) c6 in 
		let c2_ = (P1.prod_monomial (P1.unit_monomial dh) c2) in 

		let c = P1.add c1 (P1.add c6_ c2_) in

		let z2 = prod ph' qh' in 
		let z0 = prod pl ql in 
		let z1 = P1.substract (P1.substract (prod (P1.add ph' pl) (P1.add qh' ql)) z2) z0 in
		let _ = Printf.printf "------ KARATSUBA\n" in 
		let _ = printf "Découpe sur %d\n" dl in 
		let _ = printf "dh vaut %d\n" dh in 
		let _ = P1.print_poly_d "P" p in 
		let _ = P1.print_poly_d "Q" q in 
		(* let _ = P1.print_poly_d "PHi" ph in  *)
		let _ = P1.print_poly_d "PLo" pl in 
		(* let _ = P1.print_poly_d "QHi" qh in  *)
		let _ = P1.print_poly_d "QLo" ql in 
		let _ = P1.print_poly_d "PH'" ph' in 
		let _ = P1.print_poly_d "QH'" qh' in 
		let _ = P1.print_poly_d "Z0" z0 in 
		let _ = P1.print_poly_d "Z1" z1 in 
		let _ = P1.print_poly_d "Z2" z2 in
		let _ = P1.print_poly_d "C1" c1 in 
		let _ = P1.print_poly_d "C2" c2 in 
		let _ = P1.print_poly_d "C3" c3 in 
		let _ = P1.print_poly_d "C4" c4 in 
		let _ = P1.print_poly_d "C5" c5 in 
		let _ = P1.print_poly_d "C6" c6 in 
		let _ = P1.print_poly_d "C2'" c2_ in 
		let _ = P1.print_poly_d "C6'" c6_ in 
		let _ = P1.print_poly_d "C" c in 
		(* c *)

		let zres = P1.add (* z0 + X^d (z2 - z1 - z0) + X^n/2 z1 *)
		(
			P1.add (* z0 + X^d (z2 - z1 - z0) *)
				z0  
				( 
					P1.prod_monomial (* X^d (z2 - z1 - z0) *)
						(P1.monomial (P1.Coefs.make 1) dh) (* X ^ d(ph) *)
						(
							P1.substract
								(P1.substract z2 z1) (* z2 - z1 *)
								z0                   (* - z0 *)
						)
				)
		)
		(
			P1.prod_monomial (* X^n/2 * z1 *)
			(P1.monomial (P1.Coefs.make 1) dl)
			z1
		) 
		in 
		let _ = P1.print_poly_d "Z" zres in
		zres

let _ = 
	let _ = Printf.printf "Produit\n" in 
	let ph = P1.add
				(P1.monomial (CoefsNum.make 5) 2)  
				(P1.monomial (CoefsNum.make 6) 3) in
	let p2 = P1.add
				(P1.monomial (CoefsNum.make 5) 3) 
				(P1.monomial (CoefsNum.make 3) 5) in
	let _ = Printf.printf "P1 = " in
	let _ = P1.print_poly ph in 
	let _ = Printf.printf "P2 = " in
	let _ = P1.print_poly p2 in 
	let p = karatsuba ph p2 in 
	let _ = Printf.printf "NAIF:\n" in
	let _ = P1.print_poly(P1.prod ph p2) in 
	let _ = Printf.printf "MALIN:\n" in
	P1.print_poly p 
