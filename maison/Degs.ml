module type Degs = sig
	type t

  val add : t -> t -> t
  val minus_ifty : t
	val minus : t -> t -> t
	val smaller : t -> t -> bool
	val equals : t -> t -> bool
  val sup : t -> t -> t
  val print : t -> unit
  val zero : t
end

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

  let zero = (0, 0, 0)
end

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

  let zero = 0
end
