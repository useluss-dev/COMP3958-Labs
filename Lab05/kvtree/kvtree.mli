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

module Make (Ord : OrderedType) : S with type key = Ord.t
