signature ILVECM = sig
  type 'a M    (* monad encapsulating programs *)
  val >>=      : 'a M * ('a -> 'b M) -> 'b M
  val ret      : 'a -> 'a M
 
  type 'a v              (* vectors *)
  type 'a e = 'a Exp.e   (* expressions *)
  val empty    : unit -> 'a v
  val tabulate : int e -> (int e -> 'a e M) -> 'a v
  val map      : ('a e -> 'b e M) -> 'a v -> 'b v
  val rev      : 'a v -> 'a v
  val tk       : int e -> 'a v -> 'a v
  val dr       : int e -> 'a v -> 'a v
  val map2     : ('a e -> 'b e -> 'c e M) -> 'a v -> 'b v -> 'c v
  val length   : 'a v -> int e
  val memoize  : 'a v -> 'a v M
  val foldl    : ('a e * 'b e -> 'b e M) -> 'b e -> 'a v -> 'b e M
  val foldr    : ('a e * 'b e -> 'b e M) -> 'b e -> 'a v -> 'b e M
  val concat   : 'a v -> 'a v -> 'a v
  type prog    (* compiled programs *)
  val runM     : int e M -> prog
  val eval     : prog -> IL.Value
  val sum      : int v -> prog
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
