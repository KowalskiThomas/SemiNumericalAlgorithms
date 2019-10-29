open Printf

module P = Polynomes.Polynome1
module Gen = Generator.MakeGenerator(P)

(* Génère n polynômes avec 1 <= k <= n tirages *)
let rec create_polys min max step =
    if max <= min
    then []
    else (Gen.generer_polynome (if max > 10 then max else 10))::(create_polys min (max - step) step)

(*
  Teste la fonction de produit f.
  Calcule f(p, p) pour chaque p de l.
  Renvoie la liste des temps de calcul.
*)
let rec test f l = match l with
| [] -> []
| t::q ->
  let tick = Sys.time () in
  let _ = f t t in
  let tock = Sys.time () in
  let time = tock -. tick in
  time::(test f q)

(* Teste le temps d'exécution de f(x, y) *)
let test_1 f x y =
  let tick = Sys.time () in
  let _ = f x y in
  let tock = Sys.time () in
  (tock -. tick)

(* Affiche le contenu d'une liste de flottants. *)
let rec print_list l = match l with
| [] -> printf "\n"
| t::q ->
    printf "%f " t;
    print_list q

(* Calcule la différence point à point entre deux listes. *)
let rec diff l1 l2 = match (l1, l2) with
| [], [] -> []
| t1::q1, t2::q2 -> (t1 -. t2)::(diff q1 q2)
| _ -> failwith "Diff"

let rec print_results polys timediffs =
  match polys, timediffs with
  | [], [] -> printf "\n"
  | [], _
  | _, [] -> failwith "List lengths mismatch"
  | p::qp, t::qt -> printf "%d %f\n" (P.length p) t; print_results qp qt

(* Nombre de polynômes à générer. *)
let min_length = 100
let max_length = 5000
let step = 100

let _ =
  (* On génère des polynômes. *)
  print_endline "Génération...";
  let polys = create_polys min_length max_length step in
  print_endline "Test...";
  print_endline "Taille | Kara - Naïf";
  (* On calcule les (pi)² avec Karatsuba. *)
  let results1 = test (P.karatsuba) polys in
  (* On calcule les (pi)² avec prod naïf *)
  let results2 = test (P.prod) polys in
  (* On calcule la différence en temps de calcul. *)
  let results = diff results1 results2 in
  (* On affiche les résultats. *)
  print_results polys results;
  ()
