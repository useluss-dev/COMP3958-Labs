module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type ('k, 'v) t

  val empty : (key, 'a) t
  val is_empty : (key, 'a) t -> bool
  val insert : key -> 'a -> (key, 'a) t -> (key, 'a) t
  val find : key -> (key, 'a) t -> 'a
  val find_opt : key -> (key, 'a) t -> 'a option
  val delete : key -> (key, 'a) t -> (key, 'a) t
  val of_list : (key * 'a) list -> (key, 'a) t
  val size : (key, 'a) t -> int
  val to_list : (key, 'a) t -> (key * 'a) list
  val to_string : (key * 'a -> string) -> (key, 'a) t -> string
end

module Make (Ord : OrderedType) = struct
  type key = Ord.t
  type ('k, 'v) t = L | N of key * 'v * (key, 'v) t * (key, 'v) t

  exception Not_found

  (** [empty] initializes an empty kvtree. *)
  let empty = L

  (** [is_empty t] evaluates whether or not the given tree [t] is empty or not
  *)
  let is_empty t = match t with L -> true | _ -> false

  (** [insert k v t] creates a new node in the tree [t] with the given key [k],
      value [v] pair and returns the updated tree. *)
  let rec insert k v t =
    match t with
    | L -> N (k, v, L, L)
    | N (k', v', l, r) when Ord.compare k k' < 0 -> N (k', v', insert k v l, r)
    | N (k', v', l, r) when Ord.compare k k' > 0 -> N (k', v', l, insert k v r)
    | N (k', v', l, r) when Ord.compare k k' = 0 -> N (k, v, l, r)
    | _ -> t

  let rec find k t =
    match t with
    | L -> raise Not_found
    | N (k', _, l, _) when Ord.compare k k' < 0 -> find k l
    | N (k', _, _, r) when Ord.compare k k' > 0 -> find k r
    | N (_, v, _, _) -> v

  (** [find_opt k t] searches the given tree [t] for the value paired with the
      given key [k] and returns an option based on if the key exists. *)
  let find_opt k t = try Some (find k t) with Not_found -> None

  (** [largest t] finds the key value pair with the largest pair in a given tree
      [t]. *)
  let rec largest t =
    match t with
    | L -> failwith "largest: empty tree"
    | N (k, v, _, L) -> (k, v)
    | N (_, _, _, r) -> largest r

  (** [kvtree_delete k t] deletes the node in the given tree [t] that has the
      given key [k]. *)
  let rec delete k t =
    match t with
    | L -> L
    | N (k', v, l, r) when Ord.compare k k' < 0 -> N (k', v, delete k l, r)
    | N (k', v, l, r) when Ord.compare k k' > 0 -> N (k', v, l, delete k r)
    | N (_, _, l, L) -> l
    | N (_, _, L, r) -> r
    | N (_, _, l, r) ->
        let succ = largest l in
        let k', v = succ in
        N (k', v, delete k' l, r)

  (** [of_list lst] takes a list [lst] of key value pairs and turns it into a
      kvtree *)
  let of_list lst =
    List.fold_left
      (fun acc x ->
        let k, v = x in
        insert k v acc)
      L lst

  (** [size t] gets the "size" (number of nodes) of the given tree [t]. *)
  let rec size t = match t with L -> 0 | N (_, _, l, r) -> 1 + size l + size r

  (** [to_list t] converts the given tree [t] into a list of key value pairs in
      ascending order. *)
  let to_list t =
    let rec aux acc t =
      let k, v = largest t in
      match t with L -> acc | _ -> aux ((k, v) :: acc) (delete k t)
    in
    aux [] t

  let rec to_string (f : key * 'a -> string) t =
    match t with
    | L -> "#"
    | N (k, v, l, r) ->
        "^(" ^ f (k, v) ^ "," ^ to_string f l ^ "," ^ to_string f r ^ ")"
end
