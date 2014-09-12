(* Finite maps using balanced AVL trees *)

functor OrderFinMap (Order : ORDER) : MONO_FINMAP =
  OrderFinMapImpl(Order)
