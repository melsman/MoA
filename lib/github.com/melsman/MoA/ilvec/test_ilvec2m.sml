structure TestILvec2m = struct

open UTest

val () = start "test_ilvec2m.sml" "structure ILvec (ilvec2m.sml)"

open ILvec infix >>= >> ::=

val e1 = tabulate Int (I 10) ret
val e2 = map Int (fn x => ret(x + I 2)) e1
val e3 = rev e2

fun tstM s ty e f = tststr s (fn () => (ppV(eval(runM ty (f()))Uv), e))
fun tstmv s ty e f = tstM s ty e (ret o f)

val () = tstM "foldr_mem" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

fun sum v = foldl (ret o op +) (I 0) v

val () = tstM "dr" Int
              let open Int in Int.toString(6+5+4+3+2) end
              (fn () => sum (dr (I 5) e3))

val () = tstM "tk" Int
              let open Int in Int.toString(11+10) end
              (fn () => sum (tk (I 2) e3))

val () = tstM "memoize" Int "2"
              (fn () => let val v = tk (I 2) e3
                        in memoize v >>= (fn v' => ret(length v'))
                        end)

val () = tstM "map2" Int
              let open Int in Int.toString(11*2+10*3) end
              (fn () => sum (tk (I 2) (map2 Int (ret o op *) e3 e2)))

val () = tstM "foldl" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => foldl (ret o op +) (I 0) e3)

val () = tstM "foldr_mem" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => memoize e3 >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

val () = tstM "concat1" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45) end
              (fn () => memoize (concat e3 e1) >>= (fn e3' => foldr (ret o op +) (I 0) e3'))

val () = tstM "concat2" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2 + 45) end
              (fn () => let val e4 = concat e3 e1
                        in foldr (ret o op +) (I 0) e4
                        end)

val () = tstM "concat3" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => let val e = concat e3 (empty Int)
                        in sum e
                        end)

val () = tstM "concat4" Int
              let open Int in Int.toString(11+10+9+8+7+6+5+4+3+2) end
              (fn () => let val e = concat (empty Int) e3
                        in sum e
                        end)

val () = tstM "mat_sum" Int
              let open Int in Int.toString(9*450 div 2)
              (* 9*(10 + 20 + 30 + 40 + 50 + 60 + 70 + 80 + 90) div 2 
               = 9*450 div 2 *)
              end
              (fn () =>
                  let val m1 = tabulate (Vec Int) (I 10) (fn i => ret(tabulate Int (I 10) (fn j => ret(i * j))))
                  in foldl (fn (r, a) => foldl (ret o op +) a r) (I 0) m1
                  end)

val () = tstM "fromList" Int
              let open Int
                  val sum_res = 11+10+9+8+7+6+5+4+3+2
              in toString(sum_res*sum_res*1000)
              end
              (fn () => 
                  foldr (ret o op +) (I 0) e3 >>=
                        (fn sum => 
                            let val v = fromList Int [sum,I 1000,sum]
                            in foldr (ret o op *) (I 1) v
                            end))                  

fun tstT s f = tstM s Bool "true" f
fun tstF s f = tstM s Bool "false" f

val v123 = fromList Int [I 1, I 2, I 3]
val () = tstT "eq_empty"
         (fn () => eq (op ==) 
                      (empty Int) (empty Int)
         )

val () = tstT "eq_true"
         (fn () => eq (op ==) 
                      v123
                      (tabulate Int (I 3) (fn x => ret(x + I 1)))
         )

val () = tstF "eq_false"
         (fn () => eq (op ==) 
                      (fromList Int [I 1, I 4, I 3])
                      (tabulate Int (I 3) (fn x => ret(x + I 1)))
         )

val () = tstF "eq_false2"
         (fn () => memoize v123 >>=
                           (fn v => eq (op ==) v
                                       (tabulate Int (I 4) (fn x => ret(x + I 1))))
         )

val () = tstT "If_V_t"
         (fn () => eq (op ==) (If(B true, v123, empty Int)) v123)

val () = tstT "If_V_f"
         (fn () => eq (op ==) (If(B false, v123, empty Int)) (empty Int))

val () = tstT "flatten"
         (fn () => flatten Int (tabulate (Vec Int) (I 4) (fn y => ret(tabulate Int y (fn x => ret(y*x))))) >>= (fn v => eq (op ==) v (fromList Int [I 0,I 0,I 2,I 0, I 3, I 6])))

val v112233 = fromList Int [I 1, I 1, I 2, I 2, I 3, I 3]
(*
val () = tstT "double"
         (fn () => eq (op ==)
                      (double v123)
                      v112233
         )
*)
val () = tstT "stride1"
         (fn () => eq (op ==)
                      (stride (I 2) v112233)
                      v123
         )

val v12 = fromList Int [I 1, I 2]
val () = tstT "stride2"
         (fn () => eq (op ==)
                      (stride (I 3) v112233)
                      v12
         )

(*
val v0246 = tabulate (I 4) (fn i => ret(i * I 2))
val v1357 = tabulate (I 4) (fn i => ret(i * I 2 + I 1))

val () = tstT "interlv1"
         (fn () => eq (op ==)
                      (interlv v0246 v1357)
                      (tabulate (I 8) ret)
         )
*)

val () = finish()
end
