(** APL style operations on multi-dimensional arrays *)

(* Scalar-extension and identity items are assumed already to be
 * resolved. *)

signature APL = sig
  type 'a t
  val scl       : 'a -> 'a t
  val unScl     : string -> 'a t -> 'a
  val vec       : 'a -> 'a list -> 'a t
  val zilde     : 'a -> 'a t
  val liftU     : ('a -> 'b) -> 'a t -> 'b t
  val liftB     : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
  val map       : ('a -> 'b) -> 'a t -> 'b t
                                                       
  val shape     : 'a t -> int t
  val reshape   : int t * 'a t -> 'a t
  val ravel     : 'a t -> 'a t
  val iota      : int t -> int t
  val each      : 'b -> ('a t -> 'b t) -> 'a t -> 'b t
  val reduce    : ('a t * 'a t -> 'a t) -> 'a t -> 'a t -> 'a t
  val scan      : ('a t * 'a t -> 'a t) -> 'a t -> 'a t -> 'a t
  val catenate  : 'a t * 'a t -> 'a t
  val cons      : 'a t * 'a t -> 'a t
  val snoc      : 'a t * 'a t -> 'a t
  val dot       : ('c t * 'c t -> 'c t) -> ('a t * 'b t -> 'c t)
                  -> 'c t -> 'a t -> 'b t -> 'c t
  val zipWith   : 'c -> ('a t * 'b t -> 'c t) -> 'a t -> 'b t -> 'c t
  val transpose : 'a t -> 'a t
  val transpose2: int t * 'a t -> 'a t
  val reverse   : 'a t -> 'a t
  val rotate    : int t * 'a t -> 'a t
  val drop      : int t * 'a t -> 'a t
  val take      : int t * 'a t -> 'a t
  val first     : 'a t -> 'a t
  val iff       : bool t * (unit -> 'a t) * (unit -> 'a t) -> 'a t

  val pr        : ('a -> string) -> 'a t -> string
end
