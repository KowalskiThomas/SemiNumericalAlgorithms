open Coeffs
open Polynome
open Printf

let _ = Random.self_init()

module Poly = PolynomeZpZ

let construct d c p = 
        Poly.add (Poly.monomial c d) p

let k = 123

module ConstInt = struct
   let value = k
end

let one = Poly.Coefs.make 1
let three = Poly.Coefs.make 3

let ( + ) = Poly.(+)

let p = construct 5 one (construct 4 three (construct 3 one (
        construct 2 three (construct 1 one (construct 6 three Poly.poly_zero)))))

let _ = Poly.print_poly p

let _ = 
    let d = (Poly.degre_median p) in 
    Poly.Degres.print d; printf "\n"

let _ = 
    let d = (Poly.degre_median p) in 
    let h, l = Poly.decoupe_deg p d in 
    Poly.print_poly h;
    Poly.print_poly l; ()


let q = Poly.karatsuba p p 
let q' = Poly.prod p p

let _ = print_newline()

let _ = 
        let q'' =  (Poly.(+)) q q' in 
        Poly.print_poly q''


let _ = Poly.print_poly q
let _ = Poly.print_poly q'

let _ = assert (q = q')