module type RING = sig
  type t
  val add_id: t
  val mult_id: t
  val add: t -> t -> t
  val mult: t -> t -> t
  val neg: t -> t
  val to_string: t -> string
  val eq: t -> t -> bool
  val is_finite: bool
  val elements: t list option
  val of_int: int -> t
end

(* Finite Ring Z2 *)
module Z2: RING = struct
  type t = Zero | One
  let add_id = Zero
  let mult_id = One
  let is_finite = true
  let elements = Some [Zero; One]
  
  let add a b = 
    match (a, b) with
    | Zero, x -> x
    | x, Zero -> x
    | One, One -> Zero
    
  let mult a b =
    match (a, b) with
    | One, One -> One
    | _ -> Zero
    
  let neg x = x
  let to_string = function
    | Zero -> "Zero"
    | One -> "One"
    
  let eq a b = a = b
  let of_int = function
    | 0 -> Zero
    | 1 -> One
    | _ -> failwith "Z2 only has elements 0 and 1"
end

(* Infinite Ring Z *)
module Z: RING with type t = int = struct
  type t = int
  let add_id = 0
  let mult_id = 1
  let is_finite = false
  let elements = None
  
  let add = ( + )
  let mult = ( * )
  let neg x = -x
  let to_string = string_of_int
  let eq = ( = )
  let of_int x = x
end

let print_ring_element (type a) (module R : RING with type t = a) (x : a) =
  print_endline (R.to_string x)

let () =
  print_ring_element (module Z2 : RING with type t = Z2.t) (Z2.of_int 1);
  print_ring_element (module Z : RING with type t = int) (Z.of_int 42)
