open Coeffs
open Polynome
open Printf

let _ = Random.self_init()

(* Tests avec les polynômes Polynome1 (Nums / Int) *)

module Poly = Polynome1

let construct d c p = Poly.add (Poly.monomial c (Poly.Degres.make d)) p

let one = Poly.Coeffs.make 1
let three = Poly.Coeffs.make 3


let p = construct 4564 one (construct 5 one (construct 4 three 
        (construct 3 one (construct 2 three 
        (construct 1 one (construct 6 three 
        Poly.poly_zero))))))

let _ = Poly.print_poly_d "P" p

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

    let q'' = Poly.add q q' in 
    Poly.print_poly q'';
    assert (q'' = (Poly.add q q'))  

