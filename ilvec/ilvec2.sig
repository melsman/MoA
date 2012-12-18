signature ILVEC = sig
  include TYPE
  type 'a M    (* monad encapsulating program construction *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M

  (* Terms *)
  type 'a t                      (* terms *)
  type 'a v    = 'a Vec t        (* vector terms *)

  type 'a NUM  = 'a Num t        (* basic term types *)
  type INT     = Int NUM
  type DOUBLE  = Double NUM
  type BOOL    = Bool t

  val I        : int -> INT
  val D        : real -> DOUBLE
  val B        : bool -> BOOL
  val +        : 'a NUM * 'a NUM -> 'a NUM
  val -        : 'a NUM * 'a NUM -> 'a NUM
  val *        : 'a NUM * 'a NUM -> 'a NUM
  val /        : 'a NUM * 'a NUM -> 'a NUM
  val %        : INT * INT -> INT
  val <        : 'a NUM * 'a NUM -> BOOL
  val <=       : 'a NUM * 'a NUM -> BOOL
  val ==       : 'a NUM * 'a NUM -> BOOL
  val max      : 'a NUM -> 'a NUM -> 'a NUM
  val min      : 'a NUM -> 'a NUM -> 'a NUM
  val ~        : 'a NUM -> 'a NUM
  val i2d      : INT -> DOUBLE
  val d2i      : DOUBLE -> INT
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

  val merge    : 'a v -> INT -> 'a t -> 'a v

  val trans    : Int Num v -> 'a v -> 'a v 
  val extend   : INT -> 'a t -> 'a v -> 'a v
  (* [extend n e v] returns a vector of length n and values taken 
   * from v (repeatedly), using e if v is the empty vector. *)

(*
  val singlezero : 'a v -> BOOL
  val singleone : 'a v -> BOOL
*)

  val shapify  : Int Num v -> Int Num v  (* eliminate 1's and reduce to [0] if the
                                          * argument contains a 0 *)

  val shapeconcat : Int Num v -> Int Num v -> Int Num v
  (* [shapeconcat v v'] assumes v' is not [1]; this property needs to be established by
   * the caller! *)

  val sub_unsafe  : 'a v -> INT -> 'a t

  (* Compiled Programs *)
  type ('a,'b) prog
  val runM     : 'b Type.T -> 'b t M -> (unit,'b) prog
  val runF     : 'a Type.T * 'b Type.T -> ('a t -> 'b t M) -> ('a,'b) prog
 
  (* Values and Evaluation *)
  type 'a V
  val Iv       : int -> Int Num V
  val unIv     : Int Num V -> int
  val Dv       : real -> Double Num V
  val unDv     : Double Num V -> real
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
