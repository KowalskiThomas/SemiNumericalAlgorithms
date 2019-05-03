open Num
open Big_int

type num =
| Int of int
| Big_int of Big_int.big_int
| Ratio of Ratio.ratio

open Polynome

module P3 = Polynome3
let _ = 
  let z = P3.poly_zero in
  let x = P3.monomial (Int(3)) (1, 0, 0) in 
  let _ = P3.print_poly z in 
  let _ = P3.print_poly x in 

  let poly_faux = P3.P((Int(5)), (0, 1, 2), (P3.P((Int(5)), (1,2,3), P3.Null))) in
  let poly1 = P3.P((Int(4)), (3, 1, 2), (P3.P((Int(6)), (1, 2, 3), P3.Null))) in
  let poly2 = P3.P((Int(5)), (2, 1, 2), (P3.P((Int(5)), (1, 2, 3), P3.Null))) in

  let _ = P3.print_poly poly1 in 
  let _ = P3.print_poly poly2 in 

  let sum = P3.add poly1 poly2 in 
  let _ = P3.print_poly sum in

  let test1 = not (P3.correct_rep poly_faux) in
  let _ =
    if not test1 
    then Printf.printf "ERROR 1\n"
    else ()
  in
  let test2 = P3.correct_rep poly1 in
  let test3 = P3.correct_rep poly2 in
  let _ =
    if not test2 || not test3
    then Printf.printf "ERROR 2\n"
    else ()
  in

  ()
