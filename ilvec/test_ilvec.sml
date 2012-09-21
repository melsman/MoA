structure TestILvec = struct

open UTest

val () = start "test_ilvec.sml" "structure ILvec"

open IL ILvec
open Exp Program infix >>= >> ::=

val e1 = tabulate (I 10) (fn x => x)
val e2 = map (fn x => x + I 2) e1
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

val () = tststr "map2" (fn () => (ILUtil.ppValue(eval(sum (tk (I 2) (map2 (fn x => fn y => x * y) e3 e2)))), 
                                  let open Int in Int.toString(11*2+10*3)
                                  end))

val () = tststr "foldl" (fn () => (ILUtil.ppValue(eval(runM(foldl (op +) (I 0) e3))), 
                                   let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                   end))

val () = tststr "foldr_mem" (fn () => 
                                let val m = memoize e3 >>= (fn e3' => foldr (op +) (I 0) e3')
                                in (ILUtil.ppValue(eval(runM m)), 
                                    let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                    end)
                                end)

val () = tststr "concat" (fn () => 
                             let val m = memoize (concat e3 e1) >>= (fn e3' => foldr (op +) (I 0) e3')
                             in (ILUtil.ppValue(eval(runM m)), 
                                 let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45)
                                 end)
                             end)
         
end
