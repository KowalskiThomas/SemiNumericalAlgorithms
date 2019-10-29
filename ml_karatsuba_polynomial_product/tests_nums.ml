open Coeffs
open Polynomes
open Printf

let _ = Random.self_init()

(* Création du module Polynômes pour l'anneau Z/kZ *)
module Poly = Polynome(Degres.Int) (Coeffs.Nums)

let empilage d c p = Poly.add (Poly.monomial c (Poly.Degres.make d)) p

let one = Poly.Coeffs.make 1
let three = Poly.Coeffs.make 3

let ( + ) = Poly.add
let ( - ) = Poly.minus
let ( * ) = Poly.prod

let p = empilage 5 one (empilage 4 three
        (empilage 3 one (empilage 2 three
        (empilage 1 one (empilage 6 three
        Poly.null)))))

let _ = printf "P = "
let _ = Poly.print_poly p

let verif_prod =
    let q = Poly.karatsuba p p in
    let q' = Poly.prod p p in

    printf "P² par Kara = ";
    Poly.print_poly q;
    printf "P² par Naïf = ";
    Poly.print_poly q';
    assert (q = q')

module G = Generator.MakeGenerator(Poly)
let verif_prod_2 = 
  print_endline "Test avec générateur";
  let p = G.generer_polynome 500 in 
  let q = G.generer_polynome 500 in
  let kara = Poly.karatsuba p q in 
  let prod = Poly.prod p q in 
  let res = Poly.minus kara prod in 
  assert (res = Poly.null);
  print_endline "OK"

let verif_plus =
  print_endline "Test addition";
  let q = Poly.karatsuba p p in
  let q' = Poly.prod p p in

  let q'' = q + q' in
  Poly.print_poly q'';
  assert (q'' = (Poly.add q q'));
  print_endline "OK"
