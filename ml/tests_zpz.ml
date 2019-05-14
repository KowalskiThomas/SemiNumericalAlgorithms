open Coeffs
open Polynome
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
module Poly = Polynome(Degs.Int) (Coeffs.CoefsZpZ(ConstK))

let construct d c p = Poly.add (Poly.monomial c d) p

let one = Poly.Coeffs.make 1
let three = Poly.Coeffs.make 3

let ( + ) = Poly.(+)
let ( - ) = Poly.(+)
let ( * ) = Poly.(+)

let p = construct 5 one (construct 4 three 
        (construct 3 one (construct 2 three 
        (construct 1 one (construct 6 three 
        Poly.poly_zero)))))

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

let verif_plus = 
    let q = Poly.karatsuba p p in 
    let q' = Poly.prod p p in 

    let q'' = q + q' in 
    Poly.print_poly q'';
    assert (q'' = (Poly.add q q'))  
