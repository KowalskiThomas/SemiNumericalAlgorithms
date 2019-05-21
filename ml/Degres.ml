let max_deg = 999

module type Degres = sig 
  (* Le type des degrés *)
  type t

  (* Comparaison *)
  val compare : t -> t -> int

  (* Degré nul *)
  val zero : t

  (* Addition *)
  val add : t -> t -> t

  (* Soustraction  *)
  val minus : t -> t -> t

  (* Calcule l'opposé *)
  val oppose : t -> t

  (* Affiche un degré *)
  val print : t -> unit

  (* Créé un degré depuis un int *)
  val make : int -> t

  (* Génère un degré aléatoire *)
  val random : unit -> t
end

module Triplets : Degres = struct
  type t = (int * int * int)

  let zero = (0, 0, 0)

  let add d1 d2 = 
    let d11, d12, d13 = d1 in 
    let d21, d22, d23 = d2 in
    d11 + d21, d12 + d22, d13 + d23

  let oppose (d1, d2, d3) = -d1, -d1, -d3

  let minus d1 d2 = add d1 (oppose d2)

  let compare d1 d2 = 
    let dx, dy, dz  = minus d1 d2 in 
    if dx = 0 then
      if dy = 0 then 
        dz
      else
        dy
    else
      dx

  let print d = let dx, dy, dz = d in Printf.printf "X^%d Y^%d Z^%d" dx dy dz

  let make i = i, 0, 0

  let random () = ((Random.int max_deg), (Random.int max_deg), (Random.int max_deg))
end

module Int : Degres = struct 
  type t = int

  let zero = 0 

  let add i j = i + j

  let oppose i = -i

  let minus i j = i - j

  let compare i j = i - j

  let print i = Printf.printf "X^%d " i

  let make i = i

  let random () = Random.int max_deg
end

