open Coeffs
open Polynomes
open Printf

let _ = Random.self_init()

(* Constante pour l'anneau Z/pZ *)
let k = 5

(* Création du module paramétrant *)
module ConstK : ConstInt =
struct
   let p = k
end

(* Création du module Polynômes pour l'anneau Z/kZ *)
module Poly = Polynome(Degres.Int) (Coeffs.CoefsZpZ(ConstK))

let empilage d c p = Poly.add (Poly.monomial c (Poly.Degres.make d)) p

let one = Poly.Coeffs.make 1
let three = Poly.Coeffs.make 3

let ( + ) = Poly.add
let ( - ) = Poly.minus
let ( * ) = Poly.prod

(* Un polynôme de test *)
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
  let p = G.generer_polynome 250 in 
  let q = G.generer_polynome 250 in
  let kara = Poly.karatsuba p q in 
  let prod = Poly.prod p q in 
  assert(kara = prod);
  print_endline "OK"

let verif_plus =
  print_endline "Test addition";
  let q = Poly.karatsuba p p in
  let q' = Poly.prod p p in

  let q'' = q + q' in
  Poly.print_poly q'';
  assert (q'' = (Poly.add q q'));
  print_endline "OK"
