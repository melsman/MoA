structure TestILvec = struct

open UTest

val () = start "test_ilvec2.sml" "structure ILvec (ilvec2.sml)"

open ILvec infix >>= >> ::=

val e1 = tabulate (I 10) (fn x => x)
val e2 = map (fn x => x + I 2) e1
val e3 = rev e2

fun tstM s e f = tststr s (fn () => (ppV(eval(runM(f()))Uv), e))
fun tstmv s e f = tstM s e (ret o f)

val () = tstM "foldr_mem" 
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

fun sum v = foldl (ret o op +) (I 0) v

val () = tstM "dr"
              let open Int in Int.toString(6+5+4+3+2) end
              (fn () => sum (dr (I 5) e3))

val () = tstM "tk"
              let open Int in Int.toString(11+10) end
              (fn () => sum (tk (I 2) e3))

val () = tstM "memoize" "2"
              (fn () => let val v = tk (I 2) e3
                        in memoize v >>= (fn v' => ret(length v'))
                        end)

val () = tstM "map2"
              let open Int in Int.toString(11*2+10*3) end
              (fn () => sum (tk (I 2) (map2 (op *) e3 e2)))

val () = tstM "foldl"
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => foldl (ret o op +) (I 0) e3)

val () = tstM "foldr_mem"
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

val () = tstM "concat"
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45) end
              (fn () => memoize (concat e3 e1) >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

val () = tstM "concat2"
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45) end
              (fn () => let val e4 = concat e3 e1
                        in foldr (ret o op +) (I 0) e4
                        end)

val () = tstM "mat_sum"
              let open Int in Int.toString(9*450 div 2)
              (* 9*(10 + 20 + 30 + 40 + 50 + 60 + 70 + 80 + 90) div 2 
               = 9*450 div 2 *)
              end
              (fn () =>
                  let val m1 = tabulate (I 10) (fn i => tabulate (I 10) (fn j => i * j))
                  in foldr (fn (r, a) => foldr (ret o op +) a r) (I 0) m1
                  end)

val () = tstM "fromList"
              let open Int
                  val sum_res = 11+10+9+8+7+6+5+4+3+2
              in toString(sum_res*sum_res*1000)
              end
              (fn () => 
                  foldr (ret o op +) (I 0) e3 >>=
                        (fn sum => 
                            let val v = fromList [sum,I 1000,sum]
                            in foldr (ret o op *) (I 1) v
                            end))                  

fun tstT s f = tstM s "true" f
fun tstF s f = tstM s "false" f

val v123 = fromList [I 1, I 2, I 3]
val () = tstT "eq_empty"
         (fn () => eq (op ==) 
                      (empty Int) (empty Int)
         )

val () = tstT "eq_true"
         (fn () => eq (op ==) 
                      v123
                      (tabulate (I 3) (fn x => x + I 1))
         )

val () = tstF "eq_false"
         (fn () => eq (op ==) 
                      (fromList [I 1, I 4, I 3])
                      (tabulate (I 3) (fn x => x + I 1))
         )

val () = tstF "eq_false2"
         (fn () => memoize v123 >>=
                           (fn v => eq (op ==) v
                                       (tabulate (I 4) (fn x => x + I 1)))
         )

val () = tstT "If_V_t"
         (fn () => eq (op ==) (If(B true, v123, empty Int)) v123)

val () = tstT "If_V_f"
         (fn () => eq (op ==) (If(B false, v123, empty Int)) (empty Int))

val () = tstT "flatten"
         (fn () => flatten Int (tabulate (I 4) (fn y => tabulate y (fn x => y * x))) >>= (fn v => eq (op ==) v (fromList[I 0,I 0,I 2,I 0, I 3, I 6])))

val v112233 = fromList [I 1, I 1, I 2, I 2, I 3, I 3]
val () = tstT "double"
         (fn () => eq (op ==)
                      (double v123)
                      v112233
         )

val () = tstT "stride1"
         (fn () => eq (op ==)
                      (stride (I 2) v112233)
                      v123
         )

val v12 = fromList[I 1, I 2]
val () = tstT "stride2"
         (fn () => eq (op ==)
                      (stride (I 3) v112233)
                      v12
         )

val v0246 = tabulate (I 4) (fn i => i * I 2)
val v1357 = tabulate (I 4) (fn i => i * I 2 + I 1)

val () = tstT "interlv1"
         (fn () => eq (op ==)
                      (interlv v0246 v1357)
                      (tabulate (I 8) (fn x => x))
         )


val () = finish()
end
