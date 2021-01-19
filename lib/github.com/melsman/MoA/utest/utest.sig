(* Auxiliary functions for test cases *)

signature UTEST = sig
  val start  : string -> string -> unit
  val finish : unit -> unit
  val tst    : string -> (unit -> bool) -> unit
  val tstopt : string -> (unit -> string option) -> unit
  val tsteq  : ('a -> string) * ('a * 'a -> bool) -> string -> (unit -> 'a * 'a) -> unit
  val tstint : string -> (unit -> int * int) -> unit
  val tststr : string -> (unit -> string * string) -> unit
  val all    : string -> ('a -> bool) -> 'a list -> unit
end
