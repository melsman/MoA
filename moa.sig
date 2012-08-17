signature APL = sig
  type 'a t

  val zilde   : unit -> 'a t
  val scl     : 'a -> 'a t
  val vec     : 'a list -> 'a t
  val iota    : int -> int t

  val siz     : 'a t -> int
  val shape   : 'a t -> int list
  val dim     : 'a t -> int

  val rav     : 'a t -> 'a t
  val reshape : int list -> 'a t -> 'a t

  val index   : int list -> 'a t -> 'a t

  val map     : ('a -> 'b) -> 'a t -> 'b t
  val fmap    : ('a -> 'b) t -> 'a -> 'b t

  val red     : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val scan    : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t

  val out     : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val sum     : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val stk     : 'a t -> 'a t -> 'a t

  val pp      : ('a -> string) -> 'a t -> string
  val eq      : ('a * 'a -> bool) -> 'a t * 'a t -> bool
end

