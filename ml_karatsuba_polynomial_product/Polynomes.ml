open Printf

open Degres
open Coeffs

module type PolynomeSig = sig
  module Coeffs : Coeffs
  module Degres : Degres

  type polynome

  (* Opération binaire *)
  type binary_op = polynome -> polynome -> polynome

  (* Polynôme vide *)
  val null : polynome

  (* Renvoie le nombre de termes dans le polynôme *)
  val length : polynome -> int

  (* Créé un monôme *)
  val monomial : Coeffs.t -> Degres.t -> polynome

  (* Addition *)
  val add : binary_op
  (* Soustraction *)
  val minus : binary_op
  (* Produit par un monôme *)
  val prod_monom : binary_op
  (* Produit naïf *)
  val prod : binary_op
  (* Produit par un monôme construit à la volée *)
  val prod_monom_empilage : Degres.t -> Coeffs.t -> polynome -> polynome
  (* Produit par un monôme unité de degré donné *)
  val prod_monom_unit : Degres.t -> polynome -> polynome

  (* Détermine le degré médian *)
  val degre_median : polynome -> Degres.t

  (* Découpe un polynôme au-delà d'un certain degré *)
  val decoupe : Degres.t -> polynome -> (polynome * polynome)

  (* Affiche un polynôme *)
  val print_poly : polynome -> unit
  (* Affiche un polynôme avec une description *)

  val print_poly_d : string -> polynome -> unit

  (* Produit par Karatsuba *)
  val karatsuba : binary_op

  (* Vérifie que la rep est correcte *)
  val correct_rep : polynome -> bool
end

module Polynome(D : Degres)(C : Coeffs) : PolynomeSig = struct
  module Coeffs = C
  module Degres = D

  type polynome = (Degres.t * Coeffs.t) list
  type binary_op = polynome -> polynome -> polynome

  exception Impossible

  (* Polynome nul = liste vide *)
  let null = []

  let rec length p = match p with
  | [] -> 0
  | h::t -> 1 + (length t)

  (* Fonction d'addition qui vérifie aussi qu'on ne créé pas des coefficients nuls *)
  let rec add p q = match p, q with
  | [], _ -> q
  | _, [] -> p
  | h::t, a::b ->
    let dp, cp = h in
    let dq, cq = a in
    let comp = Degres.compare dp dq in
    if comp = 0 then
      let c2 = Coeffs.add cp cq in
      if c2 = Coeffs.zero then
        add t b
      else
        (dp, c2)::(add t b)
      else if comp > 0 then
        h::(add t q)
      else
        a::(add p b)

  let rec print_poly p = match p with
  | [] -> Printf.printf "\n"
  | [(d,c)] -> Coeffs.print c; Degres.print d; print_poly[]
  | (d,c)::t -> Coeffs.print c; Degres.print d; Printf.printf " + "; print_poly t

  let print_poly_d s p =
    printf "%s = " s; print_poly p

  let monomial c d = (d, c)::[]

  let rec oppose p = match p with
  | [] -> []
  | (d,c)::t -> (d,Coeffs.oppose c)::(oppose t)

  (* A - B = A + (-B) *)
  let minus p q = add p (oppose q)

  let rec prod_monom_empilage d c p =
  if c = Coeffs.zero then [] else
  match p with
  | [] -> []
  | h::t -> let dp, cp = h in
    let c2 = Coeffs.prod c cp in
    if c2 <> Coeffs.zero
    then (Degres.add dp d, c2)::(prod_monom_empilage d c t)
    else prod_monom_empilage d c t

  let prod_monom_unit d p = prod_monom_empilage d (Coeffs.make 1) p

  let prod_monom p p1 = match p1 with
  | [] -> []
  | (d, c)::[] -> prod_monom_empilage d c p
  | _ -> raise Not_found

  (* Produit naïf par convolution *)
  let prod p q =
    let rec aux a b acc =
      match a with
      | [] -> acc
      | h::t -> let d, c = h
    in
    aux t b (add (prod_monom_empilage d c b) acc)
    in aux p q []

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
    let rec aux a i = match a with
      | [] -> raise Impossible
      | (d, c)::t -> if i = 0 then d
      else aux t (i - 1)
    in aux p (n / 2 - 1)

  (* Découpe un polynôme selon un degré donné.
   * Inclut le terme de degré d dans la partie
   * haute.
   *)
   let rec decoupe d p = match p with
   | [] -> [],[]
   | h::t -> let dp, cp = h in
   let comp = Degres.compare dp d in
   if comp >= 0
   then
     let q, r = decoupe d t in
     (Degres.minus dp d, cp)::q, r
   else
     [], p

  (* Multiplication par Karatsuba *)
  let rec karatsuba p q =
    let np = length p in
    let nq = length q in
    if np <= 1 then
      prod_monom q p
    else if nq <= 1 then
      prod_monom p q
    else
      let d = if np > nq then degre_median p else degre_median q in
      let p1, p0 = decoupe d p in
      let q1, q0 = decoupe d q in
      let c2 = karatsuba p1 q1 in
      let c0 = karatsuba p0 q0 in
      let c1 = karatsuba (add p1 p0) (add q1 q0) in
      (* Opérateur pratique *)
      let ($) r deg = prod_monom_unit d r in
      add c0 (add (minus c1 (add c0 c2) $ d) ((c2 $ d) $ d))

  (* Vérifie que la représentation d'un polynôme est correcte. *)
  let rec correct_rep p =
    let rec correct_rep_aux p d_last = match p with
      | [] -> true
      | (d_cur, _)::p' ->
        if Degres.compare d_cur d_last < 0
        then correct_rep_aux p' d_cur
        else false
    in
    match p with
      | [] -> true
      | (d, _)::p' -> correct_rep_aux p' d

end

(* Quelques exemples *)
module Polynome3 = Polynome(Degres.Triplets)(Coeffs.Nums)
module Polynome1 = Polynome(Degres.Int)(Coeffs.Nums)

module Const5 : Coeffs.ConstInt = struct
  let p = 5
end
module PolynomeZ5Z = Polynome(Degres.Int)(Coeffs.CoefsZpZ(Const5))
