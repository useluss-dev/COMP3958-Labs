(** [zip_tr lst1 lst2] takes 2 lists containing any type and returns a list of
    tuples containing the values from lst1 and lst2 paired by index; not
    tail-recursive*)
let rec zip lst1 lst2 =
  match (lst1, lst2) with
  | x1 :: xs1, x2 :: xs2 -> (x1, x2) :: zip xs1 xs2
  | _ -> []

(** [zip_tr lst1 lst2] takes 2 lists containing any type and returns a list of
    tuples containing the values from lst1 and lst2 paired by index;
    tail-recursive*)
let zip_tr lst1 lst2 =
  let rec aux lst1 lst2 acc =
    match (lst1, lst2) with
    | x1 :: xs1, x2 :: xs2 -> aux xs1 xs2 ((x1, x2) :: acc)
    | _ -> List.rev acc
  in
  aux lst1 lst2 []

(**/**)

let test_zip =
  assert (zip [ 1 ] [ 'a' ] = [ (1, 'a') ]);
  assert (zip [ 1; 2 ] [ 'a'; 'b' ] = [ (1, 'a'); (2, 'b') ]);
  assert (zip [ 1; 2 ] [ 'a'; 'b'; 'c' ] = [ (1, 'a'); (2, 'b') ]);
  assert (
    zip [ 1; 2; 3; 4 ] [ 'a'; 'b'; 'c' ] = [ (1, 'a'); (2, 'b'); (3, 'c') ])

let test_zip_tr =
  assert (zip_tr [ 1 ] [ 'a' ] = [ (1, 'a') ]);
  assert (zip_tr [ 1; 2 ] [ 'a'; 'b' ] = [ (1, 'a'); (2, 'b') ]);
  assert (zip_tr [ 1; 2 ] [ 'a'; 'b'; 'c' ] = [ (1, 'a'); (2, 'b') ]);
  assert (
    zip_tr [ 1; 2; 3; 4 ] [ 'a'; 'b'; 'c' ] = [ (1, 'a'); (2, 'b'); (3, 'c') ])

(**/**)

(** [unzip lst] takes a list of tuples and returns a pair of lists (lst1,lst2).
    lst1 contains the first values of the pairs and lst2 contains the second
    values of the pairs; not tail-recursive *)
let rec unzip lst =
  match lst with
  | (x1, x2) :: xs ->
      let lst1, lst2 = unzip xs in
      (x1 :: lst1, x2 :: lst2)
  | _ -> ([], [])

(** [unzip_tr lst] takes a list of tuples and returns a pair of lists
    (lst1,lst2). lst1 contains the first values of the pairs and lst2 contains
    the second values of the pairs; tail-recursive *)
let unzip_tr lst =
  let rec aux lst acc =
    let lst1, lst2 = acc in
    match lst with
    | (x1, x2) :: xs -> aux xs (x1 :: lst1, x2 :: lst2)
    | _ -> acc
  in
  aux (List.rev lst) ([], [])

(**/**)

let test_unzip =
  assert (unzip [ (1, 'a') ] = ([ 1 ], [ 'a' ]));
  assert (unzip [ (1, 'a'); (2, 'b') ] = ([ 1; 2 ], [ 'a'; 'b' ]));
  assert (
    unzip [ (1, 'a'); (2, 'b'); (3, 'c') ] = ([ 1; 2; 3 ], [ 'a'; 'b'; 'c' ]))

let test_unzip_tr =
  assert (unzip_tr [ (1, 'a') ] = ([ 1 ], [ 'a' ]));
  assert (unzip_tr [ (1, 'a'); (2, 'b') ] = ([ 1; 2 ], [ 'a'; 'b' ]));
  assert (
    unzip_tr [ (1, 'a'); (2, 'b'); (3, 'c') ] = ([ 1; 2; 3 ], [ 'a'; 'b'; 'c' ]))

(**/**)

(** [dedup lst] returns a list with the back to back repeated values in lst
    removed; not tail-recursive*)
let rec dedup lst =
  match lst with
  | x1 :: x2 :: xs ->
      if x1 = x2 then dedup (x1 :: xs) else x1 :: dedup (x2 :: xs)
  | [ x ] -> lst
  | _ -> []

(** [dedup lst] returns a list with the back to back repeated values in lst
    removed; tail-recursive*)
let dedup_tr lst =
  let rec aux lst acc =
    match lst with
    | x1 :: x2 :: xs ->
        if x1 = x2 then aux (x1 :: xs) acc else aux (x2 :: xs) (x1 :: acc)
    | [ x ] -> List.rev (x :: acc)
    | _ -> acc
  in
  aux lst []

(**/**)

let test_dedup =
  assert (dedup [ 1; 1 ] = [ 1 ]);
  assert (dedup [ 1; 1; 2; 3; 2; 1 ] = [ 1; 2; 3; 2; 1 ]);
  assert (dedup [ 1; 1; 1; 2; 3 ] = [ 1; 2; 3 ])

let test_dedup_tr =
  assert (dedup_tr [ 1; 1 ] = [ 1 ]);
  assert (dedup_tr [ 1; 1; 2; 3; 2; 1 ] = [ 1; 2; 3; 2; 1 ]);
  assert (dedup_tr [ 1; 1; 1; 2; 3 ] = [ 1; 2; 3 ])

let run_all_tests =
  test_zip;
  test_zip_tr;
  test_unzip;
  test_unzip_tr;
  test_dedup;
  test_dedup_tr

(**/**)
