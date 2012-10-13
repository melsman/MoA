signature ILMOA = sig
  include ILVEC
  type 'a MVec
  type 'a m = 'a MVec t  (* MoA vectors *)

  val zilde   : 'a T -> 'a m
  val scl     : 'a t -> 'a m
  val vec     : 'a v -> 'a m
  val iota    : INT -> Int m

  val siz     : 'a m -> INT
  val shape   : 'a m -> Int v
  val dim     : 'a m -> INT

  val rav     : 'a m -> 'a m
  val reshape : Int v -> 'a m -> 'a m M
  val index   : Int v -> 'a m -> 'a m M
  val mmap    : ('a t -> 'b t) -> 'a m -> 'b m

  val red     : ('a t * 'b t -> 'b t M) -> 'b t -> 'a m -> 'b t M

  val meq     : ('a t * 'a t -> BOOL) -> 'a m -> 'a m -> BOOL M  

  val mif     : BOOL * 'a m * 'a m -> 'a m

  val out     : 'c T -> ('a t * 'b t -> 'c t) -> 'a m -> 'b m -> 'c m M

  val sum     : 'c T -> ('a t * 'b t -> 'c t) -> 'a m -> 'b m -> 'c m M

  val scan    : ('a t * 'b t -> 'a t) -> 'a t -> 'b m -> 'a m M

(*
  val fmap    : ('a -> 'b) m -> 'a -> 'b m

  val stk     : 'a m -> 'a m -> 'a m

  val pp      : ('a -> string) -> 'a t -> string
  val eq      : ('a * 'a -> bool) -> 'a t * 'a t -> bool
*)
end
