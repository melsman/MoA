(* Basic vectors *)

signature VEC = sig
  type 'a t
  val eq       : ('a * 'a -> bool) -> 'a t * 'a t -> bool
  val single   : 'a -> 'a t
  val empty    : unit -> 'a t
  val fromList : 'a list -> 'a t
  val tk       : int -> 'a t -> 'a t
  val dr       : int -> 'a t -> 'a t
  val length   : 'a t -> int
  val foldl    : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val foldr    : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val map      : ('a -> 'b) -> 'a t -> 'b t
  val map2     : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val fmap     : ('a -> 'b) t -> 'a -> 'b t
  val list     : 'a t -> 'a list
  val concat   : 'a t -> 'a t -> 'a t
  val flatten  : 'a t t -> 'a t
  val tabulate : int -> (int -> 'a) -> 'a t
  val sub      : 'a t * int -> 'a
  val memoize  : 'a t -> 'a t
end
