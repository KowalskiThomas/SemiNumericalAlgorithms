open Big_int

let to_string x = string_of_big_int x

let print x = Printf.printf "%s" (to_string x)

let bi x = big_int_of_int x


module Polynome = struct
  type degre = int * int * int
  type distributed_polys = 
      Null
    | P of big_int * degre * distributed_polys

  let poly_zero = Null

  let monomial coef deg = P(coef, deg, Null)



  let rec print_poly_aux p = match p with
    | Null -> Printf.printf "\n"
    | P(coef, (dx, dy, dz), p') ->
      let _ = Printf.printf "%s * " (to_string coef) in
      let _ = print_degre (dx, dy, dz) in 
      let _ = 
        if p' <> Null 
        then Printf.printf " + " 
        else ()
      in
      let _ = print_poly_aux p' in ()

  let print_poly p = match p with
    | Null -> Printf.printf "0\n"
    | p -> print_poly_aux p

  let leading_coefficient p = match p with
    | Null -> zero_big_int
    | P(x, _, _) -> x

  let reductum p = match p with
    | Null -> poly_zero
    | P(_, _, p') -> p'

  let degree p = match p with
    | Null -> 0, 0, 0
    | P(_, d, _) -> d

  let rec correct_rep p = 
    let rec correct_rep_aux p d_last = match p with
      | Null -> true
      | P(_, d_cur, p') -> 
        if lex_smaller d_cur d_last
        then correct_rep_aux p' d_cur
        else false
    in
    match p with
      | Null -> true
      | P(_, d, p') -> correct_rep_aux p' d

  let rec equals p1 p2 = match p1, p2 with
    | Null, Null -> true
    | Null, _ | _, Null -> false
    | P(c1, d1, p1'), P(c2, d2, p2') ->
      if c1 <> c2 
      then false
      else if not (deg_equals d1 d2)
      then false
      else equals p1' p2'

  let rec add p1 p2 = match p1, p2 with
    | Null, p | p, Null -> p
    | P(c1, d1, p1'), P(c2, d2, p2') -> 
      if deg_equals d1 d2 
      then P(add_big_int c1 c2, d1, add p1' p2')
      else if lex_smaller d1 d2
      then P(c2, d2, add p1 p2')
      else P(c1, d1, add p1' p2)

end

let _ = 
  let z = Polynome.poly_zero in
  let x = Polynome.monomial (bi 3) (1, 0, 0) in 
  let _ = Polynome.print_poly z in 
  let _ = Polynome.print_poly x in 

  let poly1 = Polynome.P((bi 5), (0, 1, 2), (Polynome.P((bi 5), (1,2,3), Polynome.Null))) in
  let poly2 = Polynome.P((bi 5), (2, 1, 2), (Polynome.P((bi 5), (1,2,3), Polynome.Null))) in

  let _ = Polynome.print_poly poly1 in 
  let _ = Polynome.print_poly poly2 in 

  let sum = Polynome.add poly1 poly2 in 
  let _ = Polynome.print_poly sum in

  let test1 = not (Polynome.correct_rep poly1) in
  let _ =
    if not test1 
    then Printf.printf "ERROR 1\n"
    else ()
  in
  let test2 = Polynome.correct_rep poly2 in
  let _ =
    if not test2
    then Printf.printf "ERROR 2\n"
    else ()
  in

  ()
