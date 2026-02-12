type record = { firstname : string; lastname : string; score : int }

(** [make_record firstname lastname score] creates a new record out of a
    provided [firstname] [lastname] and [score]. *)
let make_record firstname lastname score = { firstname; lastname; score }

(** [parse s] parses the provided string [s] and turns it into a new record.
    Returns an option, Some if successfully parsed, None otherwise. *)
let parse s =
  try
    Scanf.sscanf s " %s %s %s " (fun fn ln sc ->
        (* Have to do it this way because using %d for `sc` would cause some 
           lines that shouldn't work to be parsed anyways. *)
        match int_of_string_opt sc with
        | Some v when v >= 0 && v <= 100 -> Some (make_record fn ln v)
        | _ -> None)
  with _ -> None

(**/**)

let test_parse =
  assert (
    parse "homer simpson 25 # bad at nuclear engineering"
    = Some { firstname = "homer"; lastname = "simpson"; score = 25 });
  assert (parse "ned flander 12abc !! invalid" = None);
  assert (parse "monty burns" = None);
  assert (
    parse "waylon smithers 100 ! high score"
    = Some { firstname = "waylon"; lastname = "smithers"; score = 100 })

(**/**)

(** [read_record_file ic] parses each line of a file provided by [ic] into a
    record. Returns a list of the new records. *)
let read_record_file ic =
  let rec aux acc =
    try
      match parse @@ input_line ic with
      | Some v -> aux @@ (v :: acc)
      | _ -> aux acc
    with _ ->
      close_in ic;
      acc
  in
  aux []

(**/**)

(* NOTE: This test case requires in the same directory as the ml file is a file
   called `test.txt` that contains the lines:
   homer simpson 25 # bad at nuclear engineering
   ned flander 12abc !! invalid
   waylon smithers 100 ! high score
   and a file `test2.txt` which is empty
*)

let test_read_record_file =
  let list_after_read =
    [
      { firstname = "waylon"; lastname = "smithers"; score = 100 };
      { firstname = "homer"; lastname = "simpson"; score = 25 };
    ]
  in
  let file_with_content = "test.txt" in
  let empty_file = "test2.txt" in
  assert (read_record_file @@ open_in file_with_content = list_after_read);
  assert (read_record_file @@ open_in empty_file = [])

(**/**)

(** [compare_records r1 r2] compares the given records [r1] and [r2]. Returns
    score comparison their scores aren't the same, lastname comparison if their
    scores are the same, and firstname comparison if both lastname and scores
    are the same.*)
let compare_records r1 r2 =
  (* compare negative version of scores so that when sorted they will be in 
     descending order. *)
  compare
    (-r1.score, r1.lastname, r1.firstname)
    (-r2.score, r2.lastname, r2.firstname)

(**/**)

let test_compare_records =
  let waylon_smithers =
    { firstname = "waylon"; lastname = "smithers"; score = 100 }
  in
  let waylon_test = { firstname = "waylon"; lastname = "test"; score = 100 } in
  let chad_smithers =
    { firstname = "chad"; lastname = "smithers"; score = 100 }
  in
  let homer_simpson =
    { firstname = "homer"; lastname = "simpson"; score = 25 }
  in
  assert (compare_records waylon_smithers homer_simpson = -1);
  assert (compare_records waylon_smithers waylon_test = -1);
  assert (compare_records waylon_smithers chad_smithers = 1)

(**/**)

(** [insert_records r lst] inserts a new record [r] into the list [lst] based on
    the [compare_records] comparator. *)
let rec insert_record r lst =
  match lst with
  | [] -> [ r ]
  | x :: xs ->
      if compare_records r x <= 0 then r :: lst else x :: insert_record r xs

(**/**)

let test_insert_record =
  let pre_insert =
    [
      { firstname = "waylon"; lastname = "smithers"; score = 100 };
      { firstname = "homer"; lastname = "simpson"; score = 25 };
    ]
  in
  let insert_1 = { firstname = "marge"; lastname = "simpson"; score = 80 } in
  let insert_2 = { firstname = "chad"; lastname = "smithers"; score = 100 } in
  let insert_3 = { firstname = "waylon"; lastname = "test"; score = 100 } in

  assert (
    insert_record insert_1 pre_insert
    = [
        { firstname = "waylon"; lastname = "smithers"; score = 100 };
        insert_1;
        { firstname = "homer"; lastname = "simpson"; score = 25 };
      ]);
  assert (
    insert_record insert_2 pre_insert
    = [
        insert_2;
        { firstname = "waylon"; lastname = "smithers"; score = 100 };
        { firstname = "homer"; lastname = "simpson"; score = 25 };
      ]);
  assert (
    insert_record insert_3 pre_insert
    = [
        { firstname = "waylon"; lastname = "smithers"; score = 100 };
        insert_3;
        { firstname = "homer"; lastname = "simpson"; score = 25 };
      ])

(**/**)

(** [sort_records lst] sorts a list of records [lst] using insertion sort. *)
let rec sort_records lst =
  match lst with [] -> [] | x :: xs -> insert_record x @@ sort_records xs

(**/**)

let test_sort_records =
  let unsorted_1 =
    [
      { firstname = "homer"; lastname = "simpson"; score = 25 };
      { firstname = "waylon"; lastname = "smithers"; score = 100 };
      { firstname = "marge"; lastname = "simpson"; score = 80 };
    ]
  in

  let unsorted_2 =
    [
      { firstname = "ned"; lastname = "flanders"; score = 90 };
      { firstname = "rod"; lastname = "flanders"; score = 90 };
      { firstname = "todd"; lastname = "flanders"; score = 90 };
    ]
  in

  let unsorted_3 =
    [
      { firstname = "bart"; lastname = "simpson"; score = 50 };
      { firstname = "homer"; lastname = "simpson"; score = 50 };
      { firstname = "abe"; lastname = "simpson"; score = 50 };
    ]
  in

  assert (sort_records [] = []);
  assert (
    sort_records [ { firstname = "lisa"; lastname = "simpson"; score = 100 } ]
    = [ { firstname = "lisa"; lastname = "simpson"; score = 100 } ]);
  (* score descending *)
  assert (
    sort_records unsorted_1
    = [
        { firstname = "waylon"; lastname = "smithers"; score = 100 };
        { firstname = "marge"; lastname = "simpson"; score = 80 };
        { firstname = "homer"; lastname = "simpson"; score = 25 };
      ]);
  (* lastname ascending *)
  assert (
    sort_records unsorted_2
    = [
        { firstname = "ned"; lastname = "flanders"; score = 90 };
        { firstname = "rod"; lastname = "flanders"; score = 90 };
        { firstname = "todd"; lastname = "flanders"; score = 90 };
      ]);
  (* firstname ascending *)
  assert (
    sort_records unsorted_3
    = [
        { firstname = "abe"; lastname = "simpson"; score = 50 };
        { firstname = "bart"; lastname = "simpson"; score = 50 };
        { firstname = "homer"; lastname = "simpson"; score = 50 };
      ])

(**/**)

(** [print_record r] prints out the record [r]. *)
let print_record r = Printf.printf "%d %s %s\n" r.score r.lastname r.firstname

(** [print_record_list lst] prints out each record in a provided list of records
    [lst]. *)
let rec print_record_list lst =
  match lst with
  | x :: xs ->
      print_record x;
      print_record_list xs
  | _ -> ()

let () =
  if Array.length Sys.argv = 1 then
    Printf.eprintf "You must pass the program a file to parse.\n"
  else
    try
      print_record_list @@ sort_records @@ read_record_file
      @@ open_in Sys.argv.(1)
    with Sys_error s -> Printf.eprintf "%s\n" s

(**/**)

let test_all =
  test_parse;
  test_read_record_file;
  test_compare_records;
  test_insert_record;
  test_sort_records

(**/**)
