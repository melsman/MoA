signature ILMOA = sig
  include ILVEC
  type 'a MVec
  type 'a m = 'a MVec t  (* MoA vectors *)

  val zilde   : 'a T -> 'a m
  val scl     : 'a t -> 'a m
  val vec     : 'a v -> 'a m
  val iota    : INT -> INT m

  val siz     : 'a m -> INT
  val shape   : 'a m -> INT v
  val dim     : 'a m -> INT

  val rav     : 'a m -> 'a m
  val reshape : INT v -> 'a m -> 'a m
  val index   : INT v -> 'a m -> 'a m
  val map     : ('a t -> 'b t) -> 'a m -> 'b m

  val red     : ('a t * 'b t -> 'b t M) -> 'b t -> 'a m -> 'b t M

(*
  val fmap    : ('a -> 'b) m -> 'a -> 'b m
  val scan    : ('a -> 'b -> 'a) -> 'a -> 'b m -> 'a m
  val out     : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  val sum     : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m

  val stk     : 'a m -> 'a m -> 'a m

  val pp      : ('a -> string) -> 'a t -> string
  val eq      : ('a * 'a -> bool) -> 'a t * 'a t -> bool
*)
end

