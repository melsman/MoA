signature ILBASE = sig
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
  val If       : BOOL * 'a t * 'a t -> 'a t
  val fromList : 'a T -> 'a t list -> 'a v

  (* Compiled Programs *)
  type ('a,'b) prog
  val runM     : 'b T -> 'b t M -> (unit,'b) prog
  val runF     : 'a T * 'b T -> ('a t -> 'b t M) -> ('a,'b) prog
  val outprog  : string -> ('a,'b)prog -> unit
 
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
  val pp_prog  : ('a,'b) prog -> string
  val ppV      : 'a V -> string
end

signature ILVEC = sig
  include ILBASE
  val tk       : INT -> 'a v -> 'a v
  val dr       : INT -> 'a v -> 'a v

  val merge    : 'a v -> INT -> 'a t -> 'a v
  val concat   : 'a v -> 'a v -> 'a v

  val foldl    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val foldr    : ('a t * 'b t -> 'b t M) -> 'b t -> 'a v -> 'b t M
  val stride   : INT -> 'a v -> 'a v
  val length   : 'a v -> INT
  val memoize  : 'a v -> 'a v M
  val sub_unsafe  : 'a v -> INT -> 'a t M

  val trans    : Int Num v -> 'a v -> 'a v 

  val extend   : INT -> 'a v -> 'a v
  (* [extend n v] returns a vector of length n and values taken 
   * from v (repeatedly), using the prototype element of v if v is 
   * the empty vector. *)

  val flatten  : 'a T -> 'a Vec v -> 'a v M
  val flattenOf : 'a v -> 'a Vec v -> 'a v M

  val tyOfV    : 'a v -> 'a T

  val rev      : 'a v -> 'a v

  val empty    : 'a T -> 'a v
  val emptyOf  : 'a v -> 'a v

  val tabulate : 'a T -> INT -> (INT -> 'a t M) -> 'a v
  val map      : 'b T -> ('a t -> 'b t M) -> 'a v -> 'b v
  val map2     : 'c T -> ('a t * 'b t -> 'c t M) -> 'a v -> 'b v -> 'c v
  val eq       : ('a t * 'a t -> BOOL) -> 'a v -> 'a v -> BOOL M  
  val single   : 'a T -> 'a t -> 'a v
  val d2i      : DOUBLE -> INT

end
