signature MONO_SET = sig
  type t
  type e  (* type of elements *)

  val empty      : t
  val singleton  : e -> t

  val size       : t -> int
  val isEmpty    : t -> bool
  val member     : e -> t -> bool
  val eq         : t -> t -> bool

  val list       : t -> e list
  val fromList   : e list -> t
  val addList    : e list -> t -> t
      (* addList l s : Add elements in list l to s. *)

  val insert     : e -> t -> t
  val remove     : e -> t -> t
  val difference : t -> t -> t
  val intersect  : t -> t -> t
  val union      : t -> t -> t
  val partition  : (e -> bool) -> t -> t * t

  val subst      : e * e -> t -> t
      (* subst (a,b) s : Substitute element b in s with element a. *)

  val fold       : (e -> 'b -> 'b) -> 'b -> t -> 'b
      (* fold f base s; folds using f over the base element. *)

  val map        : (e -> e) -> t -> t
      (* map f s; builds a new set by applying f to each element in s *)

  val apply      : (e -> unit) -> t -> unit
      (* apply f s; applies f to each element of s (in order) *)
end
