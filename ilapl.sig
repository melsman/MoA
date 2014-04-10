signature ILAPL = sig
  include ILBASE
  type 'a m (* APL multi-dimensional arrays *)

  val addi    : INT * INT -> INT
  val subi    : INT * INT -> INT
  val muli    : INT * INT -> INT
  val divi    : INT * INT -> INT
  val lti     : INT * INT -> BOOL
  val leqi    : INT * INT -> BOOL
  val eqi     : INT * INT -> BOOL
  val maxi    : INT -> INT -> INT
  val mini    : INT -> INT -> INT
  val negi    : INT -> INT
  val addd    : DOUBLE * DOUBLE -> DOUBLE
  val subd    : DOUBLE * DOUBLE -> DOUBLE
  val muld    : DOUBLE * DOUBLE -> DOUBLE
  val divd    : DOUBLE * DOUBLE -> DOUBLE
  val ltd     : DOUBLE * DOUBLE -> BOOL
  val leqd    : DOUBLE * DOUBLE -> BOOL
  val eqd     : DOUBLE * DOUBLE -> BOOL
  val maxd    : DOUBLE -> DOUBLE -> DOUBLE
  val mind    : DOUBLE -> DOUBLE -> DOUBLE
  val negd    : DOUBLE -> DOUBLE

  val zilde   : 'a T -> 'a m
  val scl     : 'a T -> 'a t -> 'a m
  val vec     : 'a v -> 'a m
  val iota    : INT -> Int Num m

  val siz     : 'a m -> INT
  val dim     : 'a m -> INT

  val rav     : 'a m -> 'a m
  val rav0    : 'a m -> 'a v

(*  val index   : Int Num v -> 'a m -> 'a m M *)
  val each    : 'a T -> 'b T -> ('a t -> 'b t M) -> 'a m -> 'b m

  val red     : 'a T -> 'b T -> ('a t * 'b t -> 'b t M) -> 'b t -> 'a m -> 'b t M

  val meq     : 'a T -> ('a t * 'a t -> BOOL) -> 'a m -> 'a m -> BOOL M  

  val mif     : BOOL * 'a m * 'a m -> 'a m
  val lett    : 'a T -> 'a t -> 'a t M
  val letm    : 'a T -> 'a m -> 'a m M

(*
  val out     : 'c T -> ('a t * 'b t -> 'c t) -> 'a m -> 'b m -> 'c m M
*)

  val sum     : 'a T -> 'b T -> 'c T -> ('a t * 'b t -> 'c t M) -> 'a m -> 'b m -> 'c m M

  val scan    : 'a T -> 'b T -> ('a t * 'b t -> 'a t) -> 'a t -> 'b m -> 'a m M

  val catenate : 'a m -> 'a m -> 'a m M

  val take    : INT -> 'a m -> 'a m
  val drop    : INT -> 'a m -> 'a m

  val mem     : 'a m -> 'a m M

  val rotate  : INT -> 'a m -> 'a m
  val reshape : Int Num v -> 'a m -> 'a m M
  val shape   : 'a m -> Int Num v

  val prod    : 'a T -> ('a t * 'a t -> 'a t M) -> ('a t * 'a t -> 'a t M) -> 'a t
                -> 'a m -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b M 

  val reduce  : 'a T -> ('a t * 'a t -> 'a t M) -> 'a t -> 'a m -> ('a t -> 'b) -> ('a m -> 'b) -> 'b M

  val transpose : 'a m -> 'a m
end
