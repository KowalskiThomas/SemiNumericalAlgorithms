open Printf
open Polynome
open Coeffs

module P1 = Polynome1
open P1

let tests_arithmetique =
  let ph = P1.monomial (CoefsNum.make 3) 2 in 
  let _ = print_poly_d "3X^2" ph in 
  let p2 = P1.prod ph ph in 
  let _ = print_poly_d "9X^4" p2 in 
  let p3 = P1.add ph p2 in 
  let _ = print_poly_d "9X^4 + 3X^2" p3 in 
  let p4 = P1.add (P1.monomial (CoefsNum.make 5) 3) p3 in 
  let _ = print_poly_d "9X^4 + 5X^3 + 3X^2" p4 in 
  let p5 = P1.prod p4 p3 in 
  let _ = print_poly_d "81X^8 + 45X^7 + 54X^6 + 15X^5 + 9X^4" p5 in 
	let p6 = P1.add (P1.monomial (CoefsNum.make 12) 14) p5 in 
  let _ = print_poly_d "P6" p6 in
  ()

let _ =
  let p = P1.monomial (CoefsNum.make 3) 2 in 
  let p2 = substract p p in
  let _ = print_poly_d "0" p2 in  
  assert (poly_zero = p2)

let _ = 
  let one = P1.Coefs.make 1 in 
  let three = P1.Coefs.make 3 in 
  let p = P(one, 5, P(three, 4, P(one,3, P(three, 2, P1.Null)))) in 
  let q = P(three, 25,P(three, 14, P(one, 13, P(one, 4, P(three, 3, P(one, 2, P1.Null)))))) in 
  let dp, ph, pl = decoupe_milieu p in 
  let _ = printf "Découpe en %d\n" dp in 
  let _ = print_poly_d "P   " p in 
  let _ = print_poly_d "P Hi" ph in 
  let _= print_poly_d "P Lo" pl in 
  let _ = printf "\n" in 
  let _ = printf "Découpe de Q en %d\n" dp in 
  let qh, ql = decoupe_deg q dp in 
  let _ = print_poly_d "Q   " q in 
  let _ = print_poly_d "Q Hi" qh in 
  let _= print_poly_d "Q Lo" ql in 
  ()
