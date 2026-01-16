(** [fact n] returns the value of n factorial (n!): 1 * 2 * 3 * 4 * ... * n *)
let fact n =
  let rec aux n acc = if n <= 0 then acc else aux (n - 1) (acc * n) in
  aux n 1

(**/**)

let test_fact =
  assert (fact 0 = 1);
  assert (fact 2 = 2);
  assert (fact 3 = 6)

(**/**)

(** [pow x n] returns the value of x^n. Note: this function only supports n
    being a whole number *)
let pow x n =
  let rec aux n acc =
    if n = 0 then acc
    else if x = 0. then 0.
    else if n > 0 then aux (n - 1) (acc *. x)
    else aux (n + 1) (acc /. x)
  in
  aux n 1.

(**/**)

let test_pow =
  (* test positive n *)
  assert (pow 0. 0 = 1.);
  assert (pow 0. 2 = 0.);
  assert (pow 2. 3 = 8.);
  (* test negative n *)
  assert (pow 0. (-1) = 0.);
  assert (pow 2. (-2) = 0.25);
  assert (pow 2. (-3) = 0.125);
  (* test decimal value of x *)
  let tolerance = 1e-6 in
  let diff = abs_float (pow 2.2 4 -. 23.4256) in
  assert (diff < tolerance)

(**/**)

(** [expo n x] returns the first n terms of the Taylor series: exp(x) = (1 + x +
    (x^2/2!) + (x^3/3!) + ... + (x^n/n!)) *)
let expo n x =
  let rec aux n fact_n acc =
    if n = 0 then acc
    else aux (n - 1) (fact_n / n) (acc +. (pow x n /. float_of_int fact_n))
  in
  aux n (fact n) 1.

(**/**)

let test_expo =
  let tolerance = 1e-6 in
  let diff = abs_float (expo 20 1. -. exp 1.) in
  assert (diff < tolerance);
  let diff = abs_float (expo 20 2. -. exp 2.) in
  assert (diff < tolerance);
  let diff = abs_float (expo 20 3. -. exp 3.) in
  assert (diff < tolerance)

let run_all_tests =
  test_fact;
  test_pow;
  test_expo

(**/**)
