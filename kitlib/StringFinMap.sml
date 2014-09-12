structure StringFinMap : MONO_FINMAP =
  OrderFinMap(struct type t = string
                     val compare = String.compare
              end)
