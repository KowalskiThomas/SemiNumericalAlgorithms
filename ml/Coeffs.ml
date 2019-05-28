open Num

module type Coeffs =
  sig
    (* Le type des coeffs *)
    type t

    (* Coefficient nul *)
    val zero : t

    (* Vérifie si un coef est nul *)
    val is_null : t -> bool

    (* Addition  *)
    val add : t -> t -> t

    (* Multiplication *)
    val prod : t -> t -> t

    (* Détermine l'opposé  *)
    val oppose : t -> t

    (* Soustraction  *)
    val minus : t -> t -> t

    (* Affiche un coef *)
    val print : t -> unit

    (* Créé un coef depuis un entier *)
    val make : int -> t

    (* Renvoie un coef aléatoire *)
    val random : unit -> t
  end

module type ConstInt =
  sig
    val p : int
  end

module CoefsZpZ(C: ConstInt) : Coeffs = struct
  (* Le p de Z/pZ *)
  let p = C.p

  (* Calcule x mod p *)
  let reduce x p =
    let result = x mod p in
    if result < 0
    then result + p
    else result

  type t = int

  let zero = 0

  let un = 1

  let is_null c = reduce c p = 0

  let add c1 c2 = reduce (c1 + c2) p

  let prod c1 c2 = reduce (c1 * c2) p

  let oppose c = reduce (-1 * c) p

  let minus c1 c2 = reduce (c1 - c2) p

  let print c = Printf.printf " %d" c

  let make i = reduce i p

  let random () = reduce (Random.int 999) p
end

module Nums : Coeffs = struct
  let zero = Int(0)

  let un = Int(1)

  type t = num

  let is_null c = compare c (Int(0)) = 0

  let add c1 c2 = add_num c1 c2

  let prod c1 c2 = mult_num c1 c2

  let oppose c = minus_num c

  let minus c1 c2 = add c1 (oppose c2)

  let print c = Printf.printf " %s" (string_of_num c)

  let make x = Int(x)

  let random () = prod (make (Random.int 9999)) (make (Random.int 99999))
end
