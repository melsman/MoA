(* Auxiliary functions for test cases *)

signature UTEST = sig
  val start : string -> string -> unit
  val finish : unit -> unit
  val tst : string -> (unit -> bool) -> unit
  val tstopt : string -> (unit -> string option) -> unit
  val all : string -> ('a -> bool) -> 'a list -> unit
end
