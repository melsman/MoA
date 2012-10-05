structure TestILvec = struct

open UTest

val () = start "test_ilvec2.sml" "structure ILvec (ilvec2.sml)"

open ILvec infix >>= >> ::=

val e1 = tabulate (I 10) (fn x => x)
val e2 = map (fn x => x + I 2) e1
val e3 = rev e2

val () = tststr "foldr_mem" (fn () => 
                                let val m = memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3')
                                in (ILUtil.ppValue(eval(runM m)), 
                                    let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2)
                                    end)
                                end)

fun sum v = runM(foldl (ret o op +) (I 0) v)

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

val () =  tststr "mat_sum" (fn () => 
                               let 
                                 val m1 = tabulate (I 10) (fn i => tabulate (I 10) (fn j => i * j))
                                 val m = foldr (fn (r, a) => 
                                                   foldr (ret o op +) a r) (I 0) m1
                               in (ILUtil.ppValue(eval(runM m)), 
                                   let open Int in Int.toString(9*450 div 2)
                                   (* 9*(10 + 20 + 30 + 40 + 50 + 60 + 70 + 80 + 90) div 2 
                                    = 9*450 div 2 *)
                                   end)
                               end)

val () =  tststr "fromList" (fn () => 
                               let 
                                 val m = foldr (ret o op +) (I 0) e3 >>=
                                         (fn sum => 
                                             let val v = fromList [sum,I 1000,sum]
                                             in foldr (ret o op *) (I 1) v
                                             end)
                               in (ILUtil.ppValue(eval(runM m)), 
                                   let open Int
                                       val sum_res = 11+10+9+8+7+6+5+4+3+2
                                   in toString(sum_res*sum_res*1000)
                                   end)
                               end)
val () = finish()
end
