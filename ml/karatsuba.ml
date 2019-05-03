open Polynome
open Coeffs

module P1 = Polynome1

let _ = 	
	let p1 = P1.monomial (CoefsNum.make 3) 2 in 
	let p2 = P1.prod p1 p1 in 
	let p3 = P1.add p1 p2 in 
	let p4 = P1.add (P1.monomial (CoefsNum.make 5) 3) p3 in 
	let p5 = P1.prod p4 p3 in 
	let p6 = P1.add (P1.monomial (CoefsNum.make 12) 14) p5 in 
	(* let d1 = P1.degre_median p1 in 
	let d2 = P1.degre_median p2 in 
	let d3 = P1.degre_median p3 in 
	let d4 = P1.degre_median p4 in *)
	let idx, d5 = P1.degre_median p6 in 
	(* let _ = P1.print_poly p1 in 
	let _ = P1.print_poly p2 in 
	let _ = P1.print_poly p3 in 
	let _ = P1.print_poly p4 in *)
	let _ = P1.print_poly p6 in 
	(* let _ = Printf.printf "%d\n" d1 in 
	let _ = Printf.printf "%d\n" d2 in 
	let _ = Printf.printf "%d\n" d3 in 
	let _ = Printf.printf "%d\n" d4 in *)
	let _ = Printf.printf "%d\n" d5 in 
	let sup, inf = P1.decoupe p6 d5 in 
	let _ = P1.print_poly sup in 
	let _ = P1.print_poly inf in 
	()


let rec karatsuba p q = 
	let idx_mp, mp = P1.degre_median p in 
	let idx_mq, mq = P1.degre_median q in 
	let _ = Printf.printf "KARATSUBA\n" in 
	let _ = P1.print_poly p in 
	let _ = P1.print_poly q in 
	let _ = Printf.printf "ip = %d mp = %d iq = %d mq = %d\n" idx_mp mp idx_mq mq in 
	if P1.is_monomial p
	then P1.prod_monomial p q 
	else if P1.is_monomial q
	then P1.prod_monomial q p
	else
		let d = min (idx_mp) (idx_mq) in 
		let p1, p0 = P1.decoupe p d in
		let q1, q0 = P1.decoupe q d in 
		let p1 = P1.reduce_degree p1 d in 
		let q1 = P1.reduce_degree q1 d in 
		let _ = Printf.printf "P1P0Q1Q0\n" in
		let _ = P1.print_poly p1 in 
		let _ = P1.print_poly p0 in 
		let _ = P1.print_poly q1 in 
		let _ = P1.print_poly q0 in 
		let z2 = karatsuba p1 q1 in 
		let z0 = karatsuba p0 q0 in 
		let z1 = P1.minus (P1.minus (karatsuba (P1.add p1 p0) (P1.add q1 q0)) z2) z0 in
		let _ = Printf.printf "Z012\n" in
		let _ = P1.print_poly z0 in 
		let _ = P1.print_poly z1 in 
		let _ = P1.print_poly z2 in
		P1.poly_zero 

let _ = 
	let _ = Printf.printf "Produit\n" in 
	let p1 = P1.add
				(P1.monomial (CoefsNum.make 5) 2)  
				(P1.monomial (CoefsNum.make 6) 3) in
	let p2 = P1.add
				(P1.monomial (CoefsNum.make 5) 3) 
				(P1.monomial (CoefsNum.make 3) 5) in
	let _ = Printf.printf "P1 et P2:\n" in
	let _ = P1.print_poly p1 in 
	let _ = P1.print_poly p2 in 
	let p = karatsuba p1 p2 in 
	let _ = Printf.printf "NAIF:\n" in
	let _ = P1.print_poly(P1.prod p1 p2) in 
	let _ = Printf.printf "MALIN:\n" in
	P1.print_poly p 

(* DIVISER PAR N/2 GROSSE MERDE *)