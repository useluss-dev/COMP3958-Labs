type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

(** [take n s] creates a list of the first [n] elemennts of a given stream [s].
*)
let rec take n (Cons (h, t)) =
  if n <= 0 then [] else h :: take (n - 1) (Lazy.force t)

(** [exp_terms x] generates a stream of exponential function terms for a value
    [x]. This calculation uses the fact that for the taylor series t_n+1 = t_n *
    x / (n + 1)*)
let exp_terms x =
  let rec aux n term =
    Cons (term, lazy (aux (n + 1) (term *. x /. float_of_int (n + 1))))
  in
  aux 0 1.

(** [sum_terms n s] sums the first [n] values in a stream [s]. *)
let sum_stream n (Cons (_, _) as s) =
  let rec aux acc n (Cons (h, t)) =
    if n <= 0 then acc else aux (acc +. h) (n - 1) (Lazy.force t)
  in
  aux 0. n s
