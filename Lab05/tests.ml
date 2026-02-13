(* This is a minimal test file.  Your program must pass these tests in order
   to get any credit for your work.
   In utop:
   #directory "_build";;
   #load "kvtree.cmo";;
   #use "tests.ml";;
   run ();;
 *)
module M = Kvtree.Make(Int);;

let t = M.of_list [(3, "three"); (2, "two"); (7, "seven"); (6, "six"); (8, "eight")]
let l = [(2, "two"); (3, "three"); (6, "six"); (7, "seven"); (8, "eight")]
let s = 
  "^(3, three, ^(2, two, #, #), ^(7, seven, ^(6, six, #, #), ^(8, eight, #, #)))"

let t2 = M.of_list [(3, 'a')]
let s2 = "^(3, a, #, #)"

let t3 = M.insert 2 'b' t2
let s3 = "^(3, a, ^(2, b, #, #), #)"

let t4 = M.of_list [(3, "3"); (7, "7"); (5, "5"); (6, "6"); (8, "8"); (9, "9")] 
let s4 = "^(3, 3, #, ^(7, 7, ^(5, 5, #, ^(6, 6, #, #)), ^(8, 8, #, ^(9, 9, #, #))))"
let t5 = M.delete 7 t4
let s5a = "^(3, 3, #, ^(6, 6, ^(5, 5, #, #), ^(8, 8, #, ^(9, 9, #, #))))"
let s5b = "^(3, 3, #, ^(8, 8, ^(5, 5, #, ^(6, 6, #, #)), ^(9, 9, #, #)))"

let run () =
  assert (M.size t = 5);
  assert (M.find 7 t = "seven");
  assert (M.find_opt 7 t = Some "seven");
  assert (M.to_list t = l);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t = s);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %c" k v) t2 = s2);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %c" k v) t3 = s3);
  assert (M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t4 = s4);
  assert (
    let s = M.to_string (fun (k, v) -> Printf.sprintf "%d, %s" k v) t5 in
    s = s5a || s = s5b)
