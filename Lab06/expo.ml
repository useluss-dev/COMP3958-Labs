type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

(** [from n] creates a lazy stream of integers starting from [n]. *)
let rec from n = Cons (n, lazy (from (n + 1)))

(** [map f s] applies the function [f] to every value in the lazy stream [s] and
    returns the new stream. *)
let rec map f (Cons (h, t)) = Cons (f h, lazy (map f (Lazy.force t)))

(** [map2 f s1 s2] applies the function [f] to every value in both streams [s1]
    and [s2] and returns the new stream. *)
let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, lazy (map2 f (Lazy.force t1) (Lazy.force t2)))

(** [factorial x] gets the value of [x]!. *)
let rec factorial x = if x = 0 then 1 else x * factorial (x - 1)

(** [take n s] creates a list of the first [n] elemennts of a given stream [s].
*)
let rec take n (Cons (h, t)) =
  if n <= 0 then [] else h :: take (n - 1) (Lazy.force t)

(** [facts n] generates a stream of factorial starting at [n]!. *)
let facts n = map (fun x -> float_of_int @@ factorial x) (from n)

(** [powers n x] generates a stream of powers starting from [n]^[x] *)
let powers n x = map (fun x -> n ** float_of_int x) (from x)

(** [exp_terms x] generates a stream of exponential function terms for a value
    [x]. *)
let exp_terms x =
  Cons (1., lazy (Cons (x, lazy (map2 ( /. ) (powers x 2) (facts 2)))))

(** [sum_terms n s] sums the first [n] values in a stream [s]. *)
let sum_stream n (Cons (_, _) as s) =
  let rec aux acc n (Cons (h, t)) =
    if n <= 0 then acc else aux (acc +. h) (n - 1) (Lazy.force t)
  in
  aux 0. n s
