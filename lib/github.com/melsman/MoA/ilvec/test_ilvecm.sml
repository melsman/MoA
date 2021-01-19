structure TestILvecm = struct

open UTest

val () = start "test_ilvecm.sml" "structure ILvecm"

open IL ILvecm
open Exp Program infix >>= >> ::=

val e1 = tabulate (I 10) (fn x => ret x)
val e2 = map (fn x => ret(x + I 2)) e1
val e3 = rev e2

val () = tststr "sum" (fn () => (ILUtil.ppValue(eval(sum e3)), 
                                 let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                 end))

val () = tststr "dr" (fn () => (ILUtil.ppValue(eval(sum (dr (I 5) e3))), 
                                let open Int in Int.toString(6+5+4+3+2)
                                end))
val () = tststr "tk" (fn () => (ILUtil.ppValue(eval(sum (tk (I 2) e3))), 
                                let open Int in Int.toString(11+10)
                                end))

val () = tststr "memoize" (fn () => 
                              let val v = tk (I 2) e3
                                  val m = memoize v >>= (fn v' => ret(length v'))
                                  val p = runM m
                              in (ILUtil.ppValue(eval p), "2")
                              end)

val () = tststr "map2" (fn () => (ILUtil.ppValue(eval(sum (tk (I 2) (map2 (fn x => fn y => ret(x * y)) e3 e2)))), 
                                  let open Int in Int.toString(11*2+10*3)
                                  end))

val () = tststr "foldl" (fn () => (ILUtil.ppValue(eval(runM(foldl (ret o op +) (I 0) e3))), 
                                   let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                   end))

val () = tststr "foldr_mem" (fn () => 
                                let val m = memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3')
                                in (ILUtil.ppValue(eval(runM m)), 
                                    let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                    end)
                                end)

val () = tststr "concat" (fn () => 
                             let val m = memoize (concat e3 e1) >>= (fn e3' => foldr (ret o op +) (I 0) e3')
                             in (ILUtil.ppValue(eval(runM m)), 
                                 let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45)
                                 end)
                             end)

val () = tststr "concat2" (fn () => 
                             let val e4 = concat e3 e1
                                 val m = foldr (ret o op +) (I 0) e4
                             in (ILUtil.ppValue(eval(runM m)), 
                                 let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45)
                                 end)
                             end)
(*
val () = tststr "matsum" (fn () => 
                             let val e = tabulate (I 10) (fn x => 
                                             tabulate (I 10) (fn y => x * y))
                                 val m = foldr (ret o op +) (I 0) e4
                             in (ILUtil.ppValue(eval(runM m)), 
                                 let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45)
                                 end)
                             end)
  *)       

val () = finish()
end
