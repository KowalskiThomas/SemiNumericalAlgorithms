open Random
open Sys

let _ = Random.self_init ()

module MakeGenerator (S : Polynome.PolynomeSig) = struct
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
