(* Finite Ring Z2 *)
module Z2: RING = struct
  type t = int
  let add_id = 0
  let mult_id = 1
  let is_finite = true
  let elements = [0; 1]

  let enforce x =
    if x = 0 || x = 1 then x
    else failwith "Z2 = {0, 1} only"
  
  let add a b = 
    match (enforce a, enforce b) with
    | 0, x -> x
    | x, 0 -> x
    | 1, 1 -> 0
    | _ -> failwith "this should never happen"
    
  let mult a b =
    match (enforce a, enforce b) with
    | 1, 1 -> 1
    | _ -> 0
    
  let neg x = enforce x
  
  let to_string = function
    | 0 -> "0"
    | 1 -> "1"
    | _ -> failwith "Z2 = {0, 1} only"
    
  let eq a b = (enforce a) = (enforce b)
  
  let of_int = function
    | 0 -> 0
    | 1 -> 1
    | _ -> failwith "Z2 = {0, 1} only"
end

(* Infinite Ring Z *)
module Z: RING with type t = int = struct
  type t = int
  let add_id = 0
  let mult_id = 1
  let is_finite = false
  let elements = []
  
  let add = ( + )
  let mult = ( * )
  let neg x = -x
  let to_string = string_of_int
  let eq = ( = )
  let of_int x = x
end

let test_z2 () =
  print_endline "\n=== Testing Z2 Ring ===";
  
  print_endline "\nElement Creation:";
  Printf.printf "of_int 0 -> %s\n" (Z2.to_string (Z2.of_int 0));
  Printf.printf "of_int 1 -> %s\n" (Z2.to_string (Z2.of_int 1));
  
  print_endline "\nAddition:";
  let test_add a b =
    let res = Z2.add (Z2.of_int a) (Z2.of_int b) in
    Printf.printf "%s + %s = %s\n" 
      (Z2.to_string (Z2.of_int a))
      (Z2.to_string (Z2.of_int b))
      (Z2.to_string res)
  in
  test_add 0 0;
  test_add 0 1;
  test_add 1 0;
  test_add 1 1;

  print_endline "\nMultiplication:";
  let test_mult a b =
    let res = Z2.mult (Z2.of_int a) (Z2.of_int b) in
    Printf.printf "%s * %s = %s\n" 
      (Z2.to_string (Z2.of_int a))
      (Z2.to_string (Z2.of_int b))
      (Z2.to_string res)
  in
  test_mult 0 0;
  test_mult 0 1;
  test_mult 1 0;
  test_mult 1 1;

  print_endline "\nNegation:";
  let test_neg a =
    let res = Z2.neg (Z2.of_int a) in
    Printf.printf "-%s = %s\n" 
      (Z2.to_string (Z2.of_int a))
      (Z2.to_string res)
  in
  test_neg 0;
  test_neg 1;

  print_endline "\nEquality:";
  let test_eq a b =
    Printf.printf "%s == %s? %b\n" 
      (Z2.to_string (Z2.of_int a))
      (Z2.to_string (Z2.of_int b))
      (Z2.eq (Z2.of_int a) (Z2.of_int b))
  in
  test_eq 0 0;
  test_eq 0 1;
  test_eq 1 1;
  test_eq 1 0;

  print_endline "\nRing Properties:";
  Printf.printf "Additive identity: %s\n" (Z2.to_string Z2.add_id);
  Printf.printf "Multiplicative identity: %s\n" (Z2.to_string Z2.mult_id);
  Printf.printf "Is finite? %b\n" Z2.is_finite;
  Printf.printf "All elements: [%s]\n" 
    (String.concat "; " (List.map Z2.to_string Z2.elements))

let test_z () =
  print_endline "\n=== Testing Z Ring ===";
  let z_element = Z.of_int 42 in
  Printf.printf "Integer 42 -> %s\n" (Z.to_string z_element);
  Printf.printf "42 + (-42) = %s\n" (Z.to_string (Z.add z_element (Z.neg z_element)));
  Printf.printf "Is finite? %b\n" Z.is_finite

let () =
  test_z2 ();
  test_z ()
