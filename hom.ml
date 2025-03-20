module Z : RING = struct
  type t = int
  let add_id = 0
  let mult_id = 1
  let add = ( + )
  let mult = ( * )
  let neg x = -x
  let to_string = string_of_int
  let eq = ( = )
  let is_finite = false
  let elements = []
  let of_int x = x
end

module Z2 : MODULE = struct
  module R = Z
  type m = int
  let zero = 0
  let add x y = (x + y) mod 2
  let neg x = (-x) mod 2
  let smul r x = (r * x) mod 2
  let eq = ( = )
  let to_string = string_of_int
  let make a b = (a + b) mod 2
end

(* Construct a Projective Resolution for M = Z/2Z *)

module FreeModule (R: RING) : MODULE with module R = R = struct
  module R = R
  type m = R.t * R.t
  let zero = (R.add_id, R.add_id)
  let add (a1, b1) (a2, b2) = (R.add a1 a2, R.add b1 b2)
  
