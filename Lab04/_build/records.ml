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

(** [read_record_file ic] parses each line of a file provided by [ic] into a
    record. Returns a list of the new records. *)
let read_record_file ic =
  let acc = [] in
  let rec aux acc =
    try
      match parse @@ input_line ic with
      | Some v -> aux @@ (v :: acc)
      | _ -> aux acc
    with _ ->
      close_in ic;
      acc
  in
  aux acc

let compare_records r1 r2 =
  let score_cmp = compare r2.score r1.score in
  if score_cmp <> 0 then score_cmp
  else
    let lastname_cmp = String.compare r2.lastname r1.lastname in
    if lastname_cmp <> 0 then lastname_cmp
    else String.compare r2.firstname r1.firstname

let rec insert_record ~cmp r lst =
  match lst with
  | [] -> [ r ]
  | x :: xs -> if cmp r x <= 0 then r :: lst else x :: insert_record ~cmp r xs

let rec sort_records ~cmp lst =
  match lst with
  | [] -> []
  | x :: xs -> insert_record ~cmp x @@ sort_records ~cmp xs

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
      print_record_list
      @@ sort_records ~cmp:compare_records
      @@ read_record_file
      @@ open_in Sys.argv.(1)
    with Sys_error s -> Printf.eprintf "%s\n" s
