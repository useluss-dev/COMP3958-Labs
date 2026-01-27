(** [map fn lst] applies the provided function [fn] to all elements of [lst] *)
let map fn lst = List.fold_right (fun x acc -> fn x :: acc) lst []

(**/**)

let test_map =
  assert (map (fun x -> x + 1) [ 1; 2; 3; 4; 5 ] = [ 2; 3; 4; 5; 6 ]);
  assert (map (fun x -> x ** x) [ 1.; 2.; 3. ] = [ 1.; 4.; 27. ]);
  assert (map (fun x -> x / 2) [ 2; 4; 6; 8 ] = [ 1; 2; 3; 4 ])

(**/**)

(** [dedup lst] returns a list with the back to back repeated values in [lst]
    removed *)
let dedup lst =
  List.fold_right
    (fun x acc -> match acc with y :: ys when y = x -> acc | _ -> x :: acc)
    lst []

(**/**)

let test_dedup =
  assert (dedup [ 1; 1 ] = [ 1 ]);
  assert (dedup [ 1; 1; 2; 3; 2; 1 ] = [ 1; 2; 3; 2; 1 ]);
  assert (dedup [ 1; 1; 1; 2; 3 ] = [ 1; 2; 3 ])

(**/**)

(** [filter fn lst] returns a list containing only the elements of [lst] whose
    value and index meet the requirements specified by [fn].*)
let filteri fn lst =
  let rec aux i lst =
    match lst with
    | x :: xs when fn i x -> x :: aux (i + 1) xs
    | _ :: xs -> aux (i + 1) xs
    | [] -> []
  in
  aux 0 lst

(**/**)

let test_filteri =
  assert (
    filteri (fun i x -> i mod 2 == 0 && x > 2) [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    = [ 3; 5; 7 ]);
  assert (filteri (fun _ x -> x > 4) [ 1; 2; 3; 4; 5 ] = [ 5 ]);
  assert (filteri (fun i _ -> i < 2) [ 1; 2; 3 ] = [ 1; 2 ])

(**/**)

(** [filter fn lst] returns a list containing only the elements of [lst] that
    meet the requirement specified in [fn]. *)
let filter fn lst = filteri (fun _ x -> fn x) lst

(**/**)

let test_filter =
  assert (filter (fun x -> x > 2) [ 1; 2; 3; 4; 5 ] = [ 3; 4; 5 ]);
  assert (filter (fun x -> x == 4) [ 1; 2; 3; 4; 4; 4 ] = [ 4; 4; 4 ]);
  assert (filter (fun x -> x mod 2 == 0) [ 1; 2; 3; 4; 5; 6 ] = [ 2; 4; 6 ])

(**/**)

(** [every n lst] returns a list of every [n]th element of [lst]. *)
let every n lst = filteri (fun i _ -> (i + 1) mod n = 0) lst

(**/**)

let test_every =
  assert (every 3 [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] = [ 3; 6; 9 ]);
  assert (every 2 [ 1; 6; 4; 2; 8 ] = [ 6; 2 ]);
  assert (every 1 [ 1; 2; 3 ] = [ 1; 2; 3 ])

(**/**)

(** [fold_while fn acc lst] folds [lst] left until [fn] returns `None` or it
    reaches the end of [lst] and stores the result in [acc]. *)
let rec fold_while fn (acc : 'acc) lst =
  match lst with
  | x :: xs -> (
      match fn acc x with Some v -> fold_while fn v xs | None -> acc)
  | _ -> acc

(**/**)

let test_fold_while =
  assert (
    fold_while
      (fun acc x -> if acc + x < 5 then Some (acc + x) else None)
      0 [ 1; 2; 3; 4 ]
    = 3);
  assert (
    fold_while
      (fun acc x -> if x < 5 then Some (x :: acc) else None)
      [] [ 2; 4; 6; 8 ]
    = [ 4; 2 ]);
  assert (
    fold_while
      (fun acc x -> if x >= 5 then Some (x + acc) else None)
      0 [ 6; 5; 4; 3; 2 ]
    = 11)

(**/**)

(** [fold_left fn acc lst] folds [lst] to the left operating on it with [fn] and
    adding the result to [acc] *)
let fold_left fn (acc : 'acc) l =
  fold_while (fun acc x -> Some (fn acc x)) acc l

(**/**)

let test_fold_left =
  assert (fold_left (fun acc x -> x :: acc) [] [ 1; 2; 3 ] = [ 3; 2; 1 ]);
  assert (fold_left (fun acc x -> acc + x) 0 [ 1; 2; 3 ] = 6);
  assert (
    fold_left (fun acc x -> (x ** 2.) :: acc) [] [ 1.; 2.; 3. ] = [ 9.; 4.; 1. ])

(**/**)

(** [sum_while_less_than n lst ] adds all the values in [lst] from left to right
    until the sum is no longer less than [n]. Returns a pair containing the
    number of elements added and the final sum.*)
let sum_while_less_than n lst =
  let add (count, acc) x =
    match acc + x with sum when sum < n -> Some (count + 1, sum) | _ -> None
  in
  fold_while add (0, 0) lst

(**/**)

let test_sum_while_less_than =
  assert (sum_while_less_than 20 [ 6; 5; 5; 3; 4 ] = (4, 19));
  assert (sum_while_less_than 6 [ 6; 5; 5; 3; 4 ] = (0, 0));
  assert (sum_while_less_than 10 [ 5; 5; 2; 4; 3 ] = (1, 5))

(**/**)
