open Printf
open Generator

(* Choix de polynômes à coeffs Nums et degrés unaires *)
module P = Polynome.Polynome1
(* Création d'un générateur de polynômes pour le module choisi. *)
module G = MakeGenerator(P)

let _ = 
    (* Génère un polynôme avec 100 tirages *)
    let p = G.generer_polynome 100 in
    printf "card(P) = %d\n" (P.length p);
    P.print_poly_d "P" p;
    (* On ne peut pas assurer que #p = 100 car 
       deux tirages peuvent donner le même degre. *)
    assert (P.length p > 80);
