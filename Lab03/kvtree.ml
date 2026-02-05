type ('k, 'v) kvtree = L | N of ('k * 'v) * ('k, 'v) kvtree * ('k, 'v) kvtree

(** [kvtree_empty] initializes an empty kvtree. *)
let kvtree_empty = L

(** [kvtree_is_empty t] evaluates whether or not the given tree [t] is empty or
    not *)
let kvtree_is_empty = function L -> true | _ -> false

(**/**)

let test_kvtree_is_empty =
  let not_empty_kvtree = N ((1, "a"), L, L) in
  assert (kvtree_is_empty kvtree_empty = true);
  assert (kvtree_is_empty not_empty_kvtree = false)

(**/**)

(** [kvtree_insert ~cmp k v t] creates a new node in the tree [t] with the given
    key [k], value [v] pair and returns the updated tree. *)
let rec kvtree_insert ~cmp k v t =
  match t with
  | L -> N ((k, v), L, L)
  | N ((k', v'), l, r) when cmp k k' < 0 ->
      N ((k', v'), kvtree_insert ~cmp k v l, r)
  | N ((k', v'), l, r) when cmp k k' > 0 ->
      N ((k', v'), l, kvtree_insert ~cmp k v r)
  | N ((k', v'), l, r) when cmp k k' = 0 -> N ((k, v), l, r)
  | _ -> t

(**/**)

let test_kvtree_insert =
  let one_insert = N ((2, "b"), L, L) in
  let two_inserts = N ((2, "b"), N ((1, "a"), L, L), L) in
  let three_inserts = N ((2, "b"), N ((1, "b"), L, L), L) in
  assert (kvtree_insert ~cmp:compare 2 "b" kvtree_empty = one_insert);
  assert (kvtree_insert ~cmp:compare 1 "a" one_insert = two_inserts);
  assert (kvtree_insert ~cmp:compare 1 "b" two_inserts = three_inserts)

(**/**)

(** [kvtree_find_opt ~cmp k t] searches the given tree [t] for the value paired
    with the given key [k] and returns an option based on if the key exists. *)
let rec kvtree_find_opt ~cmp k t =
  match t with
  | L -> None
  | N ((k', _), l, _) when cmp k k' < 0 -> kvtree_find_opt ~cmp k l
  | N ((k', _), _, r) when cmp k k' > 0 -> kvtree_find_opt ~cmp k r
  | N ((_, v), _, _) -> Some v

(**/**)

let test_kvtree_find_opt =
  let tree = N ((2, "b"), N ((1, "a"), L, L), L) in
  assert (kvtree_find_opt ~cmp:compare 1 tree = Some "a");
  assert (kvtree_find_opt ~cmp:compare 3 tree = None)

(**/**)

let rec kvtree_largest t =
  match t with
  | L -> failwith "kvtree_largest: empty tree"
  | N ((k, v), _, L) -> (k, v)
  | N ((_, _), _, r) -> kvtree_largest r

(**/**)

let test_kvtree_largest =
  let tree = N ((2, "b"), N ((1, "a"), L, L), L) in
  assert (kvtree_largest tree = (2, "b"));
  let tree = kvtree_insert ~cmp:compare 3 "c" tree in
  assert (kvtree_largest tree = (3, "c"))

(**/**)

(** [kvtree_delete ~cmp k t] deletes the node in the given tree [t] that has the
    given key [k]. *)
let rec kvtree_delete ~cmp k t =
  match t with
  | L -> L
  | N ((k', v), l, r) when cmp k k' < 0 -> N ((k', v), kvtree_delete ~cmp k l, r)
  | N ((k', v), l, r) when cmp k k' > 0 -> N ((k', v), l, kvtree_delete ~cmp k r)
  | N ((_, _), l, L) -> l
  | N ((_, _), L, r) -> r
  | N ((_, _), l, r) ->
      let succ = kvtree_largest l in
      let k', _ = succ in
      N (succ, kvtree_delete ~cmp k' l, r)

(**/**)

let test_kvtree_delete =
  let tree =
    N ((4, "d"), N ((2, "b"), N ((1, "a"), L, L), N ((3, "c"), L, L)), L)
  in
  let one_delete = N ((4, "d"), N ((1, "a"), L, N ((3, "c"), L, L)), L) in
  let two_deletes = N ((4, "d"), N ((3, "c"), L, L), L) in
  assert (kvtree_delete ~cmp:compare 4 kvtree_empty = L);
  assert (kvtree_delete ~cmp:compare 2 tree = one_delete);
  assert (kvtree_delete ~cmp:compare 1 one_delete = two_deletes)

(**/**)

(** [kvtree_of_list lst] takes a comparator [~cmp] and a list [lst] of key value
    pairs and turns it into a kvtree *)
let rec kvtree_of_list ~cmp lst =
  List.fold_left
    (fun acc x ->
      let k, v = x in
      kvtree_insert ~cmp k v acc)
    L lst

(**/**)

let test_kvtree_of_list =
  let tree1 =
    N ((4, "d"), N ((2, "b"), N ((1, "a"), L, L), N ((3, "c"), L, L)), L)
  in
  let tree2 =
    N ((2, "b"), N ((1, "a"), L, L), N ((4, "d"), N ((3, "c"), L, L), L))
  in
  let tree3 =
    N ((3, "c"), N ((1, "a"), L, N ((2, "b"), L, L)), N ((4, "d"), L, L))
  in
  assert (
    kvtree_of_list ~cmp:compare [ (4, "d"); (2, "b"); (3, "c"); (1, "a") ]
    = tree1);
  assert (
    kvtree_of_list ~cmp:compare [ (2, "b"); (4, "d"); (3, "c"); (1, "a") ]
    = tree2);
  assert (
    kvtree_of_list ~cmp:compare [ (3, "c"); (1, "a"); (4, "d"); (2, "b") ]
    = tree3)

let test_all =
  test_kvtree_is_empty;
  test_kvtree_insert;
  test_kvtree_find_opt;
  test_kvtree_largest;
  test_kvtree_delete;
  test_kvtree_of_list

(**/**)
