signature ILVEC = sig
  type 'a M    (* monad encapsulating program construction *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  type Int and Bool and 'a Vec     (* Types *)

  type 'a T                        (* Type constructors *)
  val Int      : Int T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T

  (* Terms *)
  type 'a t                 (* terms *)
  type 'a v    = 'a Vec t   (* vector terms *)

  type INT     = Int t      (* basic terms *)
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
  val double   : 'a v -> 'a v
  val stride   : INT -> 'a v -> 'a v
  val interlv  : 'a v -> 'a v -> 'a v
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

  (* Compiled Programs *)
  type ('a,'b) prog
  val runM     : 'b t M -> (unit,'b) prog
  val runF     : ('a t -> 'b t M) -> ('a,'b) prog
 
  (* Values and Evaluation *)
  type 'a V
  val Iv       : int -> Int V
  val unIv     : Int V -> int
  val Bv       : bool -> Bool V
  val unBv     : Bool V -> bool
  val Vv       : 'a V list -> 'a Vec V
  val unVv     : 'a Vec V -> 'a V list
  val Uv       : unit V 
  val eval     : ('a,'b) prog -> 'a V -> 'b V
  val ppV      : 'a V -> string
end
(*
  val fmap     : t -> IL.Exp -> t
  val list     : 'a t -> 'a list IL.P
  val sub      : 'a t * int -> 'a
end
*)
