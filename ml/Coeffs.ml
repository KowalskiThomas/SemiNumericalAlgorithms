open Big_int
open Num

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
end

module CoefsZpZ : Coefs with type t = (int*int) = struct
	type t = (int * int)

	let reduce (p, n) = (p, n mod p)
	
	let zero = (0, 0)

	let is_zero couple = 
		let _, n = reduce couple in
		n = 0

	let add (p, x) (p', y) = 
		assert (p = p');
		reduce (p, x+y)

	let sub (p, x) (p', y) = 
		assert (p = p');
		reduce (p, x - y)

	let multiply (p, x) (p', y) =
		assert (p = p');
		reduce (p, x * y)

	let to_string (p, x) = string_of_int x

	let make x = (1, x)

	let change_base (_, x) p = (p, x)

	let divide k n = failwith "divde"

end