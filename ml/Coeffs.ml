open Big_int
open Num

let random_int () = Random.int 999999

module type Coefs = sig
	type t

	val zero : t

	val is_zero : t -> bool

	val add : t -> t -> t

	val sub : t -> t -> t

	val multiply : t -> t -> t

	val divide : t -> t -> t 

	val to_string : t -> string

	val make : int -> t

	val random : unit -> t
end

module CoefsInt : Coefs with type t = big_int = struct
	type t = big_int

	let zero = zero_big_int

	let is_zero x = x = zero_big_int

	let add x y = add_big_int x y

	let sub x y = sub_big_int x y

	let multiply x y = mult_big_int x y

	let to_string x = string_of_big_int x

	let divide x y = div_big_int x y

	let make x = big_int_of_int x

	let random () = multiply
			(multiply 
				(big_int_of_int (random_int ())) 
				(big_int_of_int (random_int ()))
			) (big_int_of_int (random_int ()))
end

module CoefsNum : Coefs with type t = num = struct
	type t = num

	let zero = Int(0)

	let is_zero x = compare_num x zero = 0

	let add x y = add_num x y

	let sub x y = sub_num x y 

	let multiply x y = mult_num x y

	let divide x y = div_num x y

	let to_string x = string_of_num x

	let make x = Num.Int(x)

	let random () = multiply
			(multiply 
				(num_of_int (random_int ())) 
				(num_of_int (random_int ()))
			) (num_of_int (random_int ()))
end

module type ConstInt = sig
	val p : int
end;;

module Const5 : ConstInt = struct
	let p = 5
end

module CoefsZpZ (I : ConstInt) = struct
	let p = I.p
	type t = int

	let reduce n = 
		let res = n mod p in 
		if res >= 0 
		then res
		else res + p
	
	let zero = 0

	let is_zero n = 
		let n = reduce n in
		n = 0

	let add n m = 
		reduce (n + m)

	let sub n m =
		reduce (n - m)

	let multiply n m = 
		reduce (n * m)

	let to_string n = string_of_int n

	let make x = x

	let divide k n = reduce (k / n)

	let random () = reduce (random_int ())
end

module Z5Z : Coefs = CoefsZpZ(Const5)
