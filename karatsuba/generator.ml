(* On s'assure d'avoir initialisé le module Random *)
let _ = Random.self_init ()

(*
 * Foncteur pour les générateurs de polynômes.
 * S : Le module des polynômes pour lequel 
 *     on veut générer des polynômes.
 *)
module MakeGenerator (S : Polynome.PolynomeSig) = struct
    module P = S

    (* Renvoie un degré aléatoire *)
    let random_deg = P.Degres.random

    (* Renvoie un coefficient aléatoire *)
    let random_coeff = P.Coeffs.random

    (* Génère un polynôme avec un nombre de tirages donnés *)
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
