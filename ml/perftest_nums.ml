open Printf

module P = Polynome.Polynome1
module Gen = Generateur.Generator(P)

let max_length = 6

let rec create_polys n =
    if n <= 0 
    then []
    else (Gen.generer_polynome n)::(create_polys (n-2))

let rec test f l = match l with
| [] -> []
| t::q -> 
    let tick = Sys.time () in 
    let _ = f t t in 
    let tock = Sys.time () in 
    let time = tock -. tick in
    (printf "%f \n" time);
    time::(test f q)

let rec print_arr l = match l with
| [] -> printf "\n" 
| t::q -> 
    printf "%f " t;
    print_arr q

let _ = 
    printf "Génération...\n";
    let polys = create_polys max_length in 
    printf "Test...\n";
    let results = [] in
    let results = test (P.karatsuba) polys in
    print_arr results