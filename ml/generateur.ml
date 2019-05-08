open Random
open Sys

let _ = Random.self_init ()

module Generator (S : Polynome.PolynomeSig) = struct
    module P = S

    let random_deg = P.Degres.random

    let random_coeff = P.Coeffs.random

    let generer_polynome (longueur : int) = 
        let rec aux (restant : int) : P.distributed_polys =
            if restant = 0
            then P.poly_zero
            else 
                let rand = (P.monomial (random_coeff ()) (random_deg ())) in
                let restant = (aux (restant - 1)) in 
                P.add rand restant
        in aux longueur
end

module P = Polynome.Polynome1 
module G = Generator(P) 

let _ = 
    let p = G.generer_polynome 100 in
    P.print_poly p 