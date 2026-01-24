(** [map fn lst] applies the provided function [fn] to all elements of [lst] *)
let map fn lst = List.fold_right (fun x acc -> fn x :: acc) lst []

(** [dedup lst] returns a list with the back to back repeated values in [lst]
    removed *)
let dedup lst =
  List.fold_right
    (fun x acc -> match acc with y :: ys when y = x -> acc | _ -> x :: acc)
    lst []

(** [filter fn lst] returns a list containing only the elements of [lst] whose
    value and index meet the requirements specified by [fn].*)
let filteri fn lst =
  let rec aux i lst =
    match lst with
    | [] -> []
    | x :: xs when fn i x -> x :: aux (i + 1) xs
    | _ :: xs -> aux (i + 1) xs
  in
  aux 0 lst

(** [filter fn lst] returns a list containing only the elements of [lst] that
    meet the requirement specified in [fn]. *)
let filter fn lst = filteri (fun _ x -> fn x) lst

(** [every n lst] returns a list of every [n]th element of [lst]. *)
let every n lst = filteri (fun i _ -> (i + 1) mod n = 0) lst

(** [fold_while fn acc lst] folds [lst] left until [fn] returns `None` or it
    reaches the end of [lst] and stores the result in [acc]. *)
let rec fold_while fn (acc : 'acc) lst =
  match lst with
  | x :: xs -> (
      match fn acc x with Some v -> fold_while fn v xs | None -> acc)
  | _ -> acc

(** [fold_left fn acc lst] folds [lst] to the left operating on it with [fn] and
    adding the result to [acc] *)
let fold_left fn (acc : 'acc) l =
  fold_while (fun acc x -> Some (fn acc x)) acc l

(** [sum_while_less_than n lst] adds all the values in [lst] from left to right
    until the sum is no longer less than [n]. *)
let sum_while_less_than n =
  let add (count, acc) x =
    match acc + x with sum when sum < n -> Some (count + 1, sum) | _ -> None
  in
  fold_while add (0, 0)
