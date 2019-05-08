open Polynome
open Printf

let _ = Random.self_init()

module P = Polynome1

let construct d c p = 
        P.add (P.monomial c d) p

let one = P.Coefs.make 1
let three = P.Coefs.make 3

let p =  construct 5 one (construct 4 three (construct 3 one (
        construct 2 three (construct 1 one (construct 6 three P.poly_zero)))))

let _ = P.print_poly p

let _ = 
    let d = (P.degre_median p) in 
    P.Degres.print d; printf "\n"

let _ = 
    let d = (P.degre_median p) in 
    let h, l = P.decoupe_deg p d in 
    P.print_poly h;
    P.print_poly l; ()


let q = P.karatsuba p p 
let q' = P.prod p p

let _ = print_newline()

let _ = 
        let q'' =  (P.(+)) q q' in 
        P.print_poly q''


let _ = P.print_poly q
let _ = P.print_poly q'

let _ = assert (q = q')