open Printf
open Generator
open Polynomes
open Coeffs
open Degres

let _ = Random.self_init()

let n = if (Array.length Sys.argv) = 1 then 3000 else (int_of_string Sys.argv.(1))
let p = 5

(* module P = MakePolynome(Int)(Nums) *)
module Const5 : ConstInt = struct
  let p = 5
end

module P = Polynome(Int)(CoefsZpZ(Const5))
module G = Generator.MakeGenerator(P)

let p = G.generer_polynome n
let q = G.generer_polynome n

let get_run_time f x y =
  let tick = Sys.time () in 
  let _ = f x y in 
  let tock = Sys.time () in 
  tock -. tick 

let _ = 
  let time_naif = get_run_time (P.prod) p q in
  let time_kara = get_run_time (P.karatsuba) p q in
  let np = P.length p in 
  printf "Nombre de tirages: %d (longueur P = %d)\n" n np;
  if time_naif < time_kara
  then printf "Naïf plus intéressant (%fs ; %f%% %f secondes de moins)\n" time_naif ((time_kara -. time_naif) /. time_kara *. 100.) (time_kara -. time_naif)
  else printf "Karatsuba plus intéressant (%fs ; %f%% %f secondes de moins)\n" time_kara ((time_naif -. time_kara) /. time_naif *. 100.) (time_naif -. time_kara) 
