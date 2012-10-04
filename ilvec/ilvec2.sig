(*
   language L0:
      Core functional programming language with immutable arrays

      e ::= x | \x.e | e e | let x=e in e
          | d | e bop e | uop e
          | tab e e | eps | map f e | rev e
          | tk e e | dr e e | map2 e e e | len e
          | foldl e e e | foldr e e e | red e e e
          | join e | memoize e

   language L1:
      L0 extended with MoA shape polymorphic arrays.
      Idea: Compile L1 into L0.
*)

signature ILVEC = sig
  type 'a M    (* monad encapsulating programs *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  type Int and Bool and 'a Vec     (* Types *)

  type 'a T                        (* Type constructors *)
  val Int      : Int T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
 
  type 'a t                 (* terms *)
  type 'a v    = 'a Vec t   (* vectors *)

  type INT     = Int t
  type BOOL    = Bool t

  val I        : int -> INT
  val B        : bool -> BOOL
  val +        : INT * INT -> INT
  val -        : INT * INT -> INT
  val *        : INT * INT -> INT
  val <        : INT * INT -> BOOL
  val <=       : INT * INT -> BOOL
  val ==       : INT * INT -> BOOL
  val max      : INT -> INT -> INT
  val min      : INT -> INT -> INT

  val empty    : 'a T -> 'a v
  val tabulate : INT -> (INT -> 'a t) -> 'a v
  val map      : ('a t -> 'b t) -> 'a v -> 'b v
  val rev      : 'a v -> 'a v
  val tk       : INT -> 'a v -> 'a v
  val dr       : INT -> 'a v -> 'a v
  val map2     : ('a t -> 'b t -> 'c t) -> 'a v -> 'b v -> 'c v
  val length   : 'a v -> INT
  val memoize  : 'a v -> 'a v M
  val foldl    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val foldr    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val concat   : 'a v -> 'a v -> 'a v

  type prog    (* compiled programs *)
  val runM     : INT M -> prog
  val eval     : prog -> IL.Value
end
(*
  val flatten  : 'a v v -> 'a v e M
  val single   : 'a -> 'a t
  val fromList : 'a list -> 'a t
  val fmap     : t -> IL.Exp -> t
  val list     : 'a t -> 'a list IL.P
  val flatten  : 'a t t -> 'a t
  val sub      : 'a t * int -> 'a
end
*)
