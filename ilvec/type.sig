signature TYPE = sig
  eqtype Int and Double and 'a Num  (* numeric types *)
     and Bool                       (* booleans *)
     and 'a Vec                     (* vectors *)

  eqtype 'a T                       (* Type constructors *)
  val Int      : Int Num T
  val Double   : Double Num T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
  val prType   : 'a T -> string
  val vecElem  : 'a Vec T -> 'a T
end
