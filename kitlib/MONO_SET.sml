(** Monomorphic sets.

The MONO_SET signature is a generic interface to monomorphic sets.
*)

signature MONO_SET = sig
  type set
  type elem
  val empty      : set
  val singleton  : elem -> set
  val size       : set -> int
  val isEmpty    : set -> bool
  val member     : set * elem -> bool
  val eq         : set * set -> bool
  val list       : set -> elem list
  val fromList   : elem list -> set
  val addList    : set * elem list -> set
  val insert     : set * elem -> set
  val remove     : set * elem -> set
  val difference : set * set -> set
  val intersect  : set * set -> set
  val union      : set * set -> set
  val partition  : (elem -> bool) -> set -> set * set
  val subst      : set * elem * elem -> set
  val fold       : (elem * 'b -> 'b) -> 'b -> set -> 'b
  val map        : (elem -> elem) -> set -> set
  val app        : (elem -> unit) -> set -> unit
end

(**

[addList (s,l)] adds elements in list l to s.

[subst (s,a,b)] substitutes element b in s with element a.

[fold f base s] folds using f over the base element.

[map f s] builds a new set by applying f to each element in s.

[app f s] applies f to each element of s (in order).

*)
