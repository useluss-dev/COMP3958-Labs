(** [digits x] takes an integer [x] and returns a list of its digits. *)
let digits x =
  let rec aux x acc =
    match x with 0 -> acc | _ -> aux (x / 10) ((x mod 10) :: acc)
  in
  aux (abs x) []

(**/**)

let test_digits =
  assert (digits 3276 = [ 3; 2; 7; 6 ]);
  assert (digits 000281 = [ 2; 8; 1 ]);
  assert (digits 0 = [])

(**/**)

(** [int_of_digits lst] takes a list [lst] of digits and turns it into an
    integer *)
let int_of_digits lst = List.fold_left (fun acc x -> (acc * 10) + x) 0 lst

(**/**)

let test_int_of_digits =
  assert (int_of_digits [ 3; 2; 7; 6 ] = 3276);
  assert (int_of_digits [ 0; 0; 3; 7; 1 ] = 371);
  assert (int_of_digits [] = 0)

(**/**)
