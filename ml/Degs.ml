(*
 * L'interface des modules représentant
 * les degrés des polynômes.
 *)
module type Degs = sig
        (* Le type des degrés *)
	type t

  (* Addition *)
  val add : t -> t -> t

  (* Constante "moins l'infini" pour polynômes nuls. *)
  val minus_ifty : t

  (* Soustraction *)
	val minus : t -> t -> t

  (* Comparaison (smaller a b renvoie true si a < b) *)
	val smaller : t -> t -> bool

  (* Test d'égalité *)
	val equals : t -> t -> bool

  (* Supremum point-à-point *)
  val sup : t -> t -> t

  (* Affiche un degré (sous la forme X^{...}) *)
	val print : t -> unit

  (* Créé un degré à partir d'un entier *)
  val make : int -> t

  (* Test à zéro *)
  val is_zero : t -> bool

  (* Renvoie un degré aléatoire *)
  val random : unit -> t
end

(*
 * Degrés représentés par des triplets d'entiers.
 *)
module TripletsInt : Degs with type t = (int * int * int) = struct
	type t = int * int * int

  let minus_ifty = -1, -1, -1

  let add (a1, b1, c1) (a2, b2, c2) =
    if a1 = -1 || a2 = -1 
    then -1, -1, -1
    else (a1 + a2, b1 + b2, c1 + c2)

  let minus (x, y, z) (x', y', z') = 
    (x - x'), (y - y'), (z - z')

  let equals (a, b, c) (d, e, f) = 
    a = d && b = e && c = f

  let smaller (a1, b1, c1) (a2, b2, c2) = 
    if a1 = a2 && b1 = b2 
    then c1 <= c2
    else if a1 = a2 
    then b1 <= b2
    else a1 <= a2

  let print (dx, dy, dz) =
    let _ = 
      if dx > 0
      then Printf.printf "X^%d" dx 
      else () in
    let _ = 
      if dy > 0
      then Printf.printf "Y^%d" dy
      else () in
    let _ = 
      if dz > 0
      then Printf.printf "Z^%d" dz 
      else () in
	()

  let sup (x, y, z) (x', y', z') =
    (max x x'), (max y y'), (max z z')

  let make x = (x,x,x)

  let is_zero x = x = (0,0,0)

  let random_int () : int = Random.int 99999

  let random () = (
    random_int (),
    random_int (),
    random_int ()
  )
end

(*
 * Degrés représentés par des entiers.
 *)
module Int : Degs with type t = int = struct
  type t = int

  let minus_ifty = -1

  let add x y = x + y

  let minus x y = x - y

  let equals x y = x = y

  let smaller x y = x <= y

  let print x =
    let _ = Printf.printf "X^%d" x in ()
  
  let sup x x' = max x x'

  let make x = x

  let is_zero x = x = 0

  let random_int () : int = Random.int 9999

  let random () = random_int ()
end
