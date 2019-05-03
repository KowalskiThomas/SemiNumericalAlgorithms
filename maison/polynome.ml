open Degs
open Coeffs
open Printf

module Polynome(D : Degs) (C : Coefs) = struct
  module Degre = D
  module Degres = Degre
  module Coefs = C

  type distributed_polys = 
      Null
    | P of Coefs.t * Degre.t * distributed_polys

  let rec filter_zero p = 
    match p with 
    | Null -> Null
    | P(k, d, p') -> 
      if Coefs.is_zero k 
      then filter_zero p'
      else P(k, d, filter_zero p')
  
  let poly_zero = Null

  let is_zero p = 
    let p' = filter_zero p in
    p' = poly_zero

  let monomial coef deg = P(coef, deg, Null)

  let unit_monomial deg = P(Coefs.make 1, deg, Null)

  let rec print_poly_aux p = 
    let p = filter_zero p in 
    match p with
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

  let print_poly_d d p = 
    let _ = printf "%s = " d in 
    print_poly p

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

  let rec equals p1 p2 = 
    let p1 = filter_zero p1 in 
    let p2 = filter_zero p2 in 
    match p1, p2 with
    | Null, Null -> true
    | Null, _ | _, Null -> false
    | P(c1, d1, p1'), P(c2, d2, p2') ->
      if c1 <> c2 
      then false
      else if not (Degre.equals d1 d2)
      then false
      else equals p1' p2'

  let rec prod_const p x = match p with
    | Null -> Null
    | P(a, d, p') -> P(Coefs.multiply a x, d, p')

  let rec add_aux p1 p2 = match p1, p2 with
    | Null, p | p, Null -> p
    | P(c1, d1, p1'), P(c2, d2, p2') -> 
      if Degre.equals d1 d2 
      then P(Coefs.add c1 c2, d1, add_aux p1' p2')
      else if Degre.smaller d1 d2
      then P(c2, d2, add_aux p1 p2')
      else P(c1, d1, add_aux p1' p2)

  let add p1 p2 = 
    let tempres = add_aux p1 p2 in 
    filter_zero tempres

  let substract p1 p2 = 
    let p2' = prod_const p2 (Coefs.make (-1)) in
    add p1 p2'

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
    | P(_, _, Null) -> 
      (* let _ = Printf.printf "IS MONOMIAL " in  *)
      (* let _ = print_poly p in  *)
      true
    | _ -> 
      (* let _ = Printf.printf "IS NOT MONOMIAL " in  *)
      (* let _ = print_poly p in  *)
      false

  let decoupe_milieu p =
    let rec aux compteur p = match p with 
    | Null -> Null, Null, Degre.minus_ifty, compteur
    | P(k, d, p') -> 
      let debut, fin, deg, total = aux (compteur + 1) p' in 
      let deg = 
        if compteur = total / 2 
        then d 
        else deg 
      in 
      if compteur > total / 2 then
        Null, P(k, d, fin), deg, total
      else
        P(k, d, debut), fin, deg, total
    in
    let debut, fin, deg, _ = aux 1 p in 
    deg, debut, fin

  let rec decoupe_deg p d = match p with
    | Null -> Null, Null
    | P(k, dp, p') -> 
      let h, l = decoupe_deg p' d in 
      if dp >= d 
      then P(k, dp, h), l 
      else Null, P(k, dp, l)

  let rec reduce_degree (p : distributed_polys) reducer = match p with
    | Null -> Null
    | P(c, d, p') -> 
      P(c, Degres.minus d reducer, reduce_degree p' reducer)
end

module Polynome3 = Polynome(Degs.TripletsInt) (Coeffs.CoefsNum)
module Polynome1 = Polynome(Degs.Int) (Coeffs.CoefsNum)
