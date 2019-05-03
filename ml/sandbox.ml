open Polynome
open Printf

let _ = Random.self_init()

open Yeti
module Poly = Polynome1

let construct d c p = 
        P1.add (P1.monomial c d) p

let one = P1.Coefs.make 1
let three = P1.Coefs.make 3

let p =  construct 5 one (construct 4 three (construct 3 one (
        construct 2 three (construct 1 one (construct 6 three Poly.poly_zero)))))

let _ = P1.print_poly p

let _ = 
    let d = (P1.degre_median p) in 
    P1.Degres.print d; printf "\n"

let _ = 
    let d = (P1.degre_median p) in 
    let h, l = P1.decoupe_deg p d in 
    P1.print_poly h;
    P1.print_poly l; ()


let q = P1.karatsuba p p 
let q' = P1.prod p p

let _ = print_newline()

let _ = 
        let q'' =  (P1.(+)) q q' in 
        P1.print_poly q''


let _ = Poly.print_poly q
let _ = Poly.print_poly q'

let _ = assert (q = q')