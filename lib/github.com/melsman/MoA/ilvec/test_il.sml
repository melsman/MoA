structure TestIL = struct

open UTest

local
  fun tstg (pr,eq) s f =
      tstopt s (fn () => let val (a,b) = f()
		         in if eq (a,b) then NONE
			    else SOME (pr a ^ " is not equal to " ^ pr b)
		         end)
in
  val tsts = tstg (fn s => s, op =)
end

val () = start "test_il.sml" "structures IL and ILUtil"

open Exp Program infix >>

val e = I 4 + (I 3 - I 2)
val _ = tsts "e" (fn () => (ILUtil.ppExp e, "5"))
val n = Name.new Type.INT
val k = Name.new Type.INT
val p = n := e >>
        For(V n, fn v => k := V v + e)
val p0 = "n0 = 5;\nfor (int n2 = 0; n2 < n0; n2++) {\nn1 = (n2+5);\n}\n"
val _ = tsts "p" (fn () => (ILUtil.ppProgram p, p0))

val () = finish()
end

(*
    c + a       --> a + c
    (a + c) + b --> (a + b) + c
    (a - c) + b --> (a + b) - c
    (c - a) + b --> (b - a) + c
    (a + c) - b --> (a - b) + c
*)
