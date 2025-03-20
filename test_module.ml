module FreeModule (R : RING) : MODULE with module R = R = struct
  module R = R
  type m = R.t * R.t

  let make a b = (a, b)
  
  let zero = make R.add_id R.add_id
  let add (a1, a2) (b1, b2) = make (R.add a1 b1) (R.add a2 b2)
  let neg (a1, a2) = make (R.neg a1) (R.neg a2)
  let smul scalar (a1, a2) = make (R.mult scalar a1) (R.mult scalar a2)
  let eq (a1, a2) (b1, b2) = R.eq a1 b1 && R.eq a2 b2
  let to_string (a, b) = Printf.sprintf "(%s, %s)" (R.to_string a) (R.to_string b)
end

module Z2 : RING with type t = int = struct
  type t = int
  let add_id = 0
  let mult_id = 1
  let is_finite = true
  let elements = [0; 1]
  
  let enforce = function
    | 0 | 1 as x -> x
    | _ -> failwith "Z2 element must be 0 or 1"
  
  let add a b = enforce ((a + b) mod 2)
  let mult a b = enforce ((a * b) mod 2)
  let neg x = x
  let eq = (=)
  let to_string = string_of_int
  let of_int = function
    | 0 -> 0 | 1 -> 1
    | _ -> failwith "Z2 elements must be 0 or 1"
end

module Z2Module = FreeModule(Z2)

let () =
  print_endline "\n=== Testing Free Module of Ring Z2===";
  
  let e0 = Z2Module.make (Z2.of_int 0) (Z2.of_int 1) in
  let e1 = Z2Module.make (Z2.of_int 1) (Z2.of_int 1) in
  
  print_endline "\n=== Element Creation ===";
  Printf.printf "e0: %s\n" (Z2Module.to_string e0);
  Printf.printf "e1: %s\n" (Z2Module.to_string e1);

  print_endline "\n=== Zero Element ===";
  let zero = Z2Module.zero in
  Printf.printf "Zero element: %s\n" (Z2Module.to_string zero);

  print_endline "\n=== Addition ===";
  let sum = Z2Module.add e0 e1 in
  Printf.printf "%s + %s = %s\n" 
    (Z2Module.to_string e0)
    (Z2Module.to_string e1)
    (Z2Module.to_string sum);

  print_endline "\n=== Negation ===";
  let neg_e1 = Z2Module.neg e1 in
  Printf.printf "-%s = %s\n" 
    (Z2Module.to_string e1)
    (Z2Module.to_string neg_e1);

  print_endline "\n=== Scalar Multiplication ===";
  let scaled1 = Z2Module.smul Z2.mult_id e0 in
  let scaled2 = Z2Module.smul Z2.add_id e1 in
  Printf.printf "1 * %s = %s\n" 
    (Z2Module.to_string e0)
    (Z2Module.to_string scaled1);
  Printf.printf "0 * %s = %s\n" 
    (Z2Module.to_string e1)
    (Z2Module.to_string scaled2);

  print_endline "\n=== Equality Checks ===";
  Printf.printf "%s == %s? %b\n" 
    (Z2Module.to_string e0)
    (Z2Module.to_string e1)
    (Z2Module.eq e0 e1);
  Printf.printf "%s == %s? %b\n" 
    (Z2Module.to_string e0)
    (Z2Module.to_string e0)
    (Z2Module.eq e0 e0);

  print_endline "\n=== Combined Operations ===";
  let complex_expr = Z2Module.add (Z2Module.smul Z2.mult_id e0) (Z2Module.neg e1) in
  Printf.printf "1*e0 + (-e1) = %s\n" (Z2Module.to_string complex_expr)
