open Printf

module P = Polynome.Polynome1
module Gen = Generator.MakeGenerator(P)

(* Génère n polynômes avec 1 <= k <= n tirages *)
let rec create_polys min max step =
    if max <= min
    then []
    else (Gen.generer_polynome max)::(create_polys min (max - step) step)

(* 
  Teste la fonction de produit f.
  Calcule f(p, p) pour chaque p de l.
  Renvoie la liste des temps de calcul. 
*)
let counter = ref(0)
let rec test f l = match l with
| [] -> []
| t::q -> 
    (* printf "%d\n" !counter; *)
    counter := !counter + 1;
    let tick = Sys.time () in 
    let _ = f t t in 
    let tock = Sys.time () in 
    let time = tock -. tick in
    time::(test f q)

let test_1 f x = 
  let tick = Sys.time () in 
  let _ = f x x in 
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
let max_length = 1500
let min_length = 1000
let step = 100

let n_max = 1000

let rec recherche n = 
  if n > n_max then printf "n too large\n" else 
  let poly = Gen.generer_polynome n in
  let t_naif = test_1 (P.prod) poly in 
  let t_kara = test_1 (P.karatsuba) poly in 
  if t_naif > t_kara 
  then let _ = printf "%f > %f : n = %d\n" t_naif t_kara n in ()
  else recherche (n + 10)

let _ = 
  let n = 90000 in 
  let poly = Gen.generer_polynome n in
  let t_naif = 0.0 in 
  let t_kara = 0.0 in 
  (* let t_naif = test_1 (P.prod) poly in *)
  print_endline "Naïf fini";
  let t_kara = test_1 (P.karatsuba) poly in 
  print_endline "Karatsuba fini";
  printf "Naif: %f / Karatsuba: %f / K - N = %f : n = %d\n" t_naif t_kara (t_kara -. t_naif) n 



(* let _ = 
  printf "Recherche bonne valeur\n";
  recherche 15 *)

(* let _ = 
    (* On génère des polynômes. *)
    printf "Génération...\n";
    let polys = create_polys min_length max_length step in 
    printf "Test...\n";
    (* On calcule les (pi)² avec Karatsuba. *)
    let results1 = test (P.karatsuba) polys in
    (* On calcule les (pi)² avec prod naïf *)
    let results2 = test (P.prod) polys in 
    (* On calcule la différence en temps de calcul. *)
    let results = diff results1 results2 in 
    (* On affiche les résultats. *)
    print_results polys results;
    ()
 *)
