type 'a infstream = Cons of 'a * (unit -> 'a infstream)

(** [from n] creates a infinite stream of integers starting at [n]. **)
let rec from n = Cons (n, fun () -> from (n + 1))

(** [take n s] creates a list of the first [n] elements of a given infinite
    stream [s]. (used for testing) **)
let rec take n (Cons (h, t)) = if n <= 0 then [] else h :: take (n - 1) (t ())

(** [filter f s] filters out all elements in a given infinite stream [s] that
    don't satisfy the predicate [f]. *)
let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ())) else filter f (t ())

(** [sieve s] removes all values in [s] who is a multiple of [h]. This is a
    method for finding prime numbers. *)
let rec sieve (Cons (h, t)) =
  Cons (h, fun () -> sieve @@ filter (fun x -> x mod h <> 0) (t ()))

(** [generate_primes] generates an infinite stream of all primes. *)
let generate_primes = sieve (from 2)
