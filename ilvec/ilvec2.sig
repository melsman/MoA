signature ILVEC = sig
  type 'a M    (* monad encapsulating program construction *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  type Int and Bool and 'a Vec     (* Types *)

  type 'a T                        (* Type constructors *)
  val Int      : Int T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
 
  type 'a t                 (* terms *)
  type 'a v    = 'a Vec t   (* vectors *)

  type INT     = Int t      (* Basic terms *)
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
  val If       : BOOL * 'a t * 'a t -> 'a t
  val empty    : 'a T -> 'a v
  val emptyOf  : 'a v -> 'a v
  val single   : 'a t -> 'a v
  val fromList : 'a t list -> 'a v
  val tabulate : INT -> (INT -> 'a t) -> 'a v
  val map      : ('a t -> 'b t) -> 'a v -> 'b v
  val rev      : 'a v -> 'a v
  val tk       : INT -> 'a v -> 'a v
  val dr       : INT -> 'a v -> 'a v
  val map2     : ('a t * 'b t -> 'c t) -> 'a v -> 'b v -> 'c v
  val length   : 'a v -> INT
  val memoize  : 'a v -> 'a v M
  val foldl    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val foldr    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val concat   : 'a v -> 'a v -> 'a v
  val eq       : ('a t * 'a t -> BOOL) -> 'a v -> 'a v -> BOOL M  
  val flatten  : 'a T -> 'a Vec v -> 'a v M
  val flattenOf  : 'a v -> 'a Vec v -> 'a v M

  val shapify  : Int v -> Int v  (* eliminate 1's and reduce to [0] if the
                                  * argument contains a 0 *)

  type prog    (* compiled programs *)
  type Value   = IL.Value
  val runM     : 'a t M -> prog
  val eval     : prog -> Value
end
(*
  val fmap     : t -> IL.Exp -> t
  val list     : 'a t -> 'a list IL.P
  val sub      : 'a t * int -> 'a
end
*)
