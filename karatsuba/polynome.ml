open Printf

open Degs
open Coeffs

(*
 * L'interface pour les modules de Polynômes.
 * Degres : Module des degrés
 * Coeffs : Module des coefficients
 *)
module type PolynomeSig = sig
  module Degres : Degs
  module Coeffs : Coefs

  (* Type représentant un polynôme. *)
  type distributed_polys
        
  (* Type de fonction "opération binaire" *)
  type binary_op = distributed_polys -> distributed_polys -> distributed_polys

  (* Polynôme nul *)
  val poly_zero : distributed_polys

  (* Supprime les composantes nulles d'un polynôme. *)
  val filter_zero : distributed_polys -> distributed_polys

  (* Test à zéro *)
  val is_zero : distributed_polys -> bool

  (* Créé un monôme de coefficient et de degré donnés. *)
  val monomial : Coeffs.t -> Degres.t -> distributed_polys

  (* Affiche un polynôme. *)
  val print_poly : distributed_polys -> unit

  (* Affiche un polynôme nommé : "%s = {...}" *)
  val print_poly_d : string -> distributed_polys -> unit

  (* Addition *)
  val add : binary_op

  (* Multiplication par Karatsuba *)
  val karatsuba : binary_op
  
  (* Multiplication naïve *)
  val prod : binary_op

  (* Renvoie le nombre de coefficients d'un polynôme. *)
  val length : distributed_polys -> int
end

(*
 * Foncteur pour les polynômes.
 * D : Degrés à utiliser
 * C : Coefficients à utiliser
 *)
module Polynome(D : Degs) (C : Coefs) = struct
  module Degres = D
  module Coeffs = C

  type distributed_polys = 
      Null
    | P of Coeffs.t * Degres.t * distributed_polys

  type binary_op = distributed_polys -> distributed_polys -> distributed_polys

  let poly_zero = Null

  let rec filter_zero p = 
    match p with 
    | Null -> Null
    | P(k, d, p') -> 
      if Coeffs.is_zero k 
      then filter_zero p'
      else P(k, d, filter_zero p')

  let is_zero p = (filter_zero p) = poly_zero

  let monomial coef deg = P(coef, deg, Null)

  let rec print_poly_aux p = match p with
    | Null -> Printf.printf "\n"
    | P(coef, d, p') ->
      let _ = Printf.printf "%s * " (Coeffs.to_string coef) in
      let _ = Degres.print d in 
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
    printf "%s = " d;
    print_poly p

  (* Coefficient dominant *)
  let leading_coefficient p = match p with
    | Null -> Coeffs.zero
    | P(x, _, _) -> x

  (* Polynôme privé de son coefficient dominant. *)
  let reductum p = match p with
    | Null -> poly_zero
    | P(_, _, p') -> p'

  (* Degré *)
  let degree p = match p with
    | Null -> Degres.minus_ifty
    | P(_, d, _) -> d

  (* Vérifie que la représentation d'un polynôme est correcte. *)
  let rec correct_rep p = 
    let rec correct_rep_aux p d_last = match p with
      | Null -> true
      | P(_, d_cur, p') -> 
        if Degres.smaller d_cur d_last
        then correct_rep_aux p' d_cur
        else false
    in
    match p with
      | Null -> true
      | P(_, d, p') -> correct_rep_aux p' d

  (* Egalité *)
  let rec equals p1 p2 = match p1, p2 with
    | Null, Null -> true
    | Null, _ | _, Null -> false
    | P(c1, d1, p1'), P(c2, d2, p2') ->
      if c1 <> c2 
      then false
      else if not (Degres.equals d1 d2)
      then false
      else equals p1' p2'

  let rec add_aux p1 p2 = match p1, p2 with
    | Null, p 
    | p, Null -> p
    | P(c1, d1, p1'), P(c2, d2, p2') -> 
      if Degres.equals d1 d2 
      then P(Coeffs.add c1 c2, d1, add_aux p1' p2')
      else if Degres.smaller d1 d2
      then P(c2, d2, add_aux p1 p2')
      else P(c1, d1, add_aux p1' p2)

  let add p1 p2 = 
    let res = add_aux p1 p2 in 
    filter_zero res

  (* Renvoie l'opposé d'un polynôme *)
  let rec oppose p = match p with
    | Null -> Null
    | P(c, d, p') -> P(Coeffs.multiply (Coeffs.make (-1)) c, d, oppose p')

  (* Soustraction *)
  let minus p q = add p (oppose q)

  (* Produit par un monôme *)
  let rec prod_monomial m p = match m with
    | Null -> Null
    | P(c, d, P(_)) -> 
      let _ = print_poly m in 
      failwith "prod_monomial used with non monomial"
    | P(cm, dm, Null) ->
      match p with 
      | Null -> Null
      | P(cp, dp, p') -> P(Coeffs.multiply cp cm, Degres.add dm dp, prod_monomial m p')

    let prod_monomial_u (d:Degres.t) (p:distributed_polys) = 
      let m = monomial (Coeffs.make 1) d in
      prod_monomial m p

  (* Produit (naïf) *)
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

  (* PPCM de deux polynômes *)
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
        P(Coeffs.make 1, Degres.sup d1 d2, Null)

  let reduce p q = match p, q with
    | _, Null -> failwith "Division par 0"
    | Null, _ -> Null
    | P(c, dp, p'), P(cq, dq, _) -> 
      if Degres.smaller dq dp 
      then P(Coeffs.divide c cq, Degres.minus dp dq, p')
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
          then P(Coeffs.divide c cq, Degres.minus dp dq, result), true
          else p, false
        else p, false
    in 
    let result, confirm = (aux p q) in 
    if confirm then result else p

  let rec reduce_list p l = match l with
    | [] -> p
    | t::q -> reduce_list (reduce p t) q 

  (* Renvoie le S-polynôme de p et q *)
  let spol p q = match p, q with
    | Null, _
    | _, Null -> failwith "Degré 0"
    | P(cp, dp, p'), P(cq, dq, q') -> 
      let d = Degres.sup dp dq in 
        minus
          (
            prod
              (
                monomial 
                  (Coeffs.divide (Coeffs.make 1) cp) 
                  (Degres.minus d dp)
              )
              p
          )
          (
            prod
              (
                monomial 
                  (Coeffs.divide (Coeffs.make 1) cq)
                  (Degres.minus d dq)
              )
              q
          )

  (* Vérifie si p est un monôme (ou 0) *)
  let is_monomial p = match p with
    | Null -> true
    | P(_, _, Null) -> true
    | _ -> false

  let rec length (p : distributed_polys) : int = match p with
    | Null -> 0
    | P(_, _, p') -> (length p') + 1

  (* 
   * Détermine le degré médian d'un polynôme.
   * - Dans le cas d'un polynôme de longueur 2,
   *   le degré médian est le plus haut.
   * - Dans le cas d'un polynôme de longueur 1,
   *   le degré médian est le plus haut.
   * - Dans le cas d'un polynôme nul, le degré
   *   médian est 0.
   *)
  let degre_median p = 
    let n = length p in
    let rec aux i p = 
      match p with
      | Null -> Degres.minus_ifty
      | P(_, d, Null) -> d 
      | P(_, d, p') ->
        if i = 0 
        then d
        else aux (i - 1) p'
    in 
      let d = aux ((n - 2) / 2) p in 
      d

  (* Découpe un polynôme selon un degré donné.
   * Inclut le terme de degré d dans la partie
   * haute.
   *)
  let decoupe_deg p d = 
    let rec aux p = match p with
    | Null -> Null, Null
    | P(c, pd, p') -> 
      let high, low = aux p' in 
      if pd >= d then
        P(c, Degres.minus pd d, high), low
      else
        high, P(c, pd, low)
    in aux p 

  (* Réduit le degré d'un polynôme. *)
  let rec reduce_degree (p : distributed_polys) reducer = match p with
    | Null -> Null
    | P(c, d, p') -> 
      P(c, Degres.minus d reducer, reduce_degree p' reducer)

  (* Produit par l'algorithme de Karatsuba. *)
  let rec karatsuba p q =
    (* print_poly_d "P" p;
    print_poly_d "Q" q;
    print_endline ""; *)
    match p, q with 
    | Null, _ -> Null
    | _, Null -> Null
    | P(_, _, Null), _ -> prod_monomial p q
    | _, P(_, _, Null) -> prod_monomial q p
    | P(_, _, P(_, _, Null)), _ -> prod p q
    | _, P(_,_, P(_,_,Null)) -> prod p q
    | _, _ -> 
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

  (* Surcharge opérateurs (ça marche des fois) *)
  let ( + ) = add 
  let ( * ) = karatsuba 
  let ( - ) = minus 

end

(* Polynômes des TPs (nums / triplets) *)
module Polynome3 : PolynomeSig = Polynome(Degs.TripletsInt) (Coeffs.CoefsNum)
(* Polynômes du projet (nums / entiers) *)
module Polynome1 : PolynomeSig = Polynome(Degs.Int) (Coeffs.CoefsNum)
(* Polynômes de Z/pZ (int / Z/5Z) *)
module PolynomeZpZ : PolynomeSig = Polynome(Degs.Int) (Coeffs.Z5Z)