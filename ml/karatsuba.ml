open Polynome
open Coeffs
open Printf

module P1 = Polynome1

let rec karatsuba p q =
  if P1.is_monomial p then 
    let p' = P1.prod_monomial q p in
    assert (P1.length p' = P1.length p); p'
  else if P1.is_monomial q then 
    let p' = P1.prod_monomial p q in 
    assert (P1.length p' = P1.length p); p'
  else 
    let d = max (P1.degre_median p) (P1.degre_median q) in
    let p1, p0 = P1.decoupe_deg p d in
    let q1, q0 = P1.decoupe_deg q d in 
    let c2 = karatsuba p1 q1 in 
    let c0 = karatsuba p0 q0 in 
    let c1 = karatsuba (P1.add p1 p0) (P1.add q1 q0) in
    P1.add 
      c0 
      (P1.add 
        (P1.prod_monomial_u d
          (P1.minus 
            c1 
            (P1.add c0 c2)
          )
        )
        (P1.prod_monomial_u d 
          (P1.prod_monomial_u d 
            c2
          )
        )
      )

