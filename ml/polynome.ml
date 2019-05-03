open Degs
open Coeffs

module Polynome(D : Degs) (C : Coefs) = struct
  module Degre = D
  module Degres = D
  module Coefs = C
  module Coeffs = C

  type distributed_polys = 
      Null
    | P of Coefs.t * Degre.t * distributed_polys

  let poly_zero = Null

  let rec filter_zero p = 
    match p with 
    | Null -> Null
    | P(k, d, p') -> 
      if Coefs.is_zero k 
      then filter_zero p'
      else P(k, d, filter_zero p')

  let is_zero p = (filter_zero p) = poly_zero

  let monomial coef deg = P(coef, deg, Null)

  let rec print_poly_aux p = match p with
    | Null -> Printf.printf "\n"
    | P(coef, d, p') ->
      let _ = Printf.printf "%s * " (Coefs.to_string coef) in
      let _ = Degre.print d in 
      let _ = 
        if p' <> Null 
        then Printf.printf " + " 
        else ()
      in
      let _ = print_poly_aux p' in ()

  let print_poly p = match p with
    | Null -> Printf.printf "0\n"
    | p -> print_poly_aux p

  let leading_coefficient p = match p with
    | Null -> Coefs.zero
    | P(x, _, _) -> x

  let reductum p = match p with
    | Null -> poly_zero
    | P(_, _, p') -> p'

  let degree p = match p with
    | Null -> Degres.minus_ifty
    | P(_, d, _) -> d

  let rec correct_rep p = 
    let rec correct_rep_aux p d_last = match p with
      | Null -> true
      | P(_, d_cur, p') -> 
        if Degre.smaller d_cur d_last
        then correct_rep_aux p' d_cur
        else false
    in
    match p with
      | Null -> true
      | P(_, d, p') -> correct_rep_aux p' d

  let rec equals p1 p2 = match p1, p2 with
    | Null, Null -> true
    | Null, _ | _, Null -> false
    | P(c1, d1, p1'), P(c2, d2, p2') ->
      if c1 <> c2 
      then false
      else if not (Degre.equals d1 d2)
      then false
      else equals p1' p2'

  let rec add_aux p1 p2 = match p1, p2 with
    | Null, p 
    | p, Null -> p
    | P(c1, d1, p1'), P(c2, d2, p2') -> 
      if Degre.equals d1 d2 
      then P(Coefs.add c1 c2, d1, add_aux p1' p2')
      else if Degre.smaller d1 d2
      then P(c2, d2, add_aux p1 p2')
      else P(c1, d1, add_aux p1' p2)

  let add p1 p2 = 
    let res = add_aux p1 p2 in 
    filter_zero res

  let rec oppose p = match p with
    | Null -> Null
    | P(c, d, p') -> P(Coefs.multiply (Coefs.make (-1)) c, d, oppose p')

  let minus p q = add p (oppose q)

  let rec prod_monomial m p = match m with
    | Null -> Null
    | P(c, d, P(_)) -> 
      let _ = print_poly m in 
      failwith "prod_monomial used with non monomial"
    | P(cm, dm, Null) ->
      match p with 
      | Null -> Null
      | P(cp, dp, p') -> P(Coefs.multiply cp cm, Degre.add dm dp, prod_monomial m p')

    let prod_monomial_u (d:Degres.t) (p:distributed_polys) = 
      let m = monomial (Coefs.make 1) d in
      prod_monomial m p

  let rec prod p1 p2 = match p1, p2 with
    | Null, p | p, Null -> Null
    | P(c1, d1, p1'), P(c2, d2, p2') ->
      (add 
        (prod_monomial 
          (monomial c1 d1) 
          p2
        ) 
        (prod p1' p2)
      )

  let ppcm m1 m2 = match m1, m2 with
    | _, Null
    | Null, _ -> Null
    | P(c1, d1, p1'), P(c2, d2, p2') ->
        (*
        let _ = assert (c1 = 1) in
        let _ = assert c2 = 1 in
        let _ = assert p1' = Null in
        let _ = assert p2' = Null in
        *)
        P(Coefs.make 1, Degres.sup d1 d2, Null)

  let reduce p q = match p, q with
    | _, Null -> failwith "Division par 0"
    | Null, _ -> Null
    | P(c, dp, p'), P(cq, dq, _) -> 
      if Degres.smaller dq dp 
      then P(Coefs.divide c cq, Degres.minus dp dq, p')
      else p

  let strong_reduce p q = 
    let rec aux p q = match p, q with
      | _, Null -> failwith "Division par 0"
      | Null, _ -> Null, true
      | P(c, dp, p'), P(cq, dq, _) -> 
        if Degres.smaller dq dp
        then 
          let result, confirm = aux p' q in
          if confirm
          then P(Coefs.divide c cq, Degres.minus dp dq, result), true
          else p, false
        else p, false
    in 
    let result, confirm = (aux p q) in 
    if confirm then result else p

  let rec reduce_list p l = match l with
    | [] -> p
    | t::q -> reduce_list (reduce p t) q 

  let spol p q = match p, q with
    | Null, _
    | _, Null -> failwith "Degré 0"
    | P(cp, dp, p'), P(cq, dq, q') -> 
      let d = Degre.sup dp dq in 
        minus
          (
            prod
              (
                monomial 
                  (Coefs.divide (Coefs.make 1) cp) 
                  (Degre.minus d dp)
              )
              p
          )
          (
            prod
              (
                monomial 
                  (Coefs.divide (Coefs.make 1) cq)
                  (Degre.minus d dq)
              )
              q
          )

  let is_monomial p = match p with
    | Null -> true
    | P(_, _, Null) -> true
    | _ -> false

  let rec length (p : distributed_polys) : int = match p with
    | Null -> 0
    | P(_, _, p') -> (length p') + 1

  let degre_median p = 
    let n = length p in
    let rec aux i p = 
      match p with
      | Null -> Degres.make 0
      | P(_, d, Null) -> Degres.make 1
      | P(_, d, p') ->
        if i = n / 2 
        then 
          if Degre.is_zero d then Degre.make 1
          else d
        else aux (i + 1) p'
    in aux 0 p

  (* let decoupe p idx_ex = 
    let rec aux p compteur = match p with
    | Null -> Null, Null
    | P(c, d, p') -> 
      if compteur = idx_ex then 
        Null, (add (P(c, d, Null)) p')
      else 
        let suite, fin = aux p' (compteur + 1) in 
        (add (P(c, d, Null)) suite), fin
    in 
    aux p 1 *)

  let decoupe_deg p d = 
    let rec aux p = match p with
    | Null -> Null, Null
    | P(c, pd, p') -> 
      let high, low = aux p' in 
      if pd >= d then
        P(c, Degre.minus pd d, high), low
      else
        high, P(c, pd, low)
    in aux p 

  let rec reduce_degree (p : distributed_polys) reducer = match p with
    | Null -> Null
    | P(c, d, p') -> 
      P(c, Degres.minus d reducer, reduce_degree p' reducer)

  let rec karatsuba p q =
    if is_monomial p then 
      let p' = prod_monomial q p in
      assert (length p' = length p); p'
    else if is_monomial q then 
      let p' = prod_monomial p q in 
      assert (length p' = length p); p'
    else 
      let d = max (degre_median p) (degre_median q) in
      let p1, p0 = decoupe_deg p d in
      let q1, q0 = decoupe_deg q d in 
      let c2 = karatsuba p1 q1 in 
      let c0 = karatsuba p0 q0 in 
      let c1 = karatsuba (add p1 p0) (add q1 q0) in
      add 
        c0 
        (add 
          (prod_monomial_u d
            (minus 
              c1 
              (add c0 c2)
            )
          )
          (prod_monomial_u d 
            (prod_monomial_u d 
              c2
            )
          )
        )

  let ( + ) = add 
  let ( * ) = karatsuba 
  let ( - ) = minus 

end

module Polynome3 = Polynome(Degs.TripletsInt) (Coeffs.CoefsNum)
module Polynome1 = Polynome(Degs.Int) (Coeffs.CoefsNum)
module PolynomeZpZ = Polynome(Degs.Int) (Coeffs.CoefsZpZ)

(* let ( + ) p1 p2 = Polynome1.add p1 p2
let ( * ) p1 p2 = Polynome1.karatsuba p1 p2
let ( - ) p1 p2 = Polynome1.minus p1 p2 *)
