structure TestILmoa = struct

open UTest

val () = start "test_ilmoa.sml" "structure ILmoa (ilmoa.sml)"

open ILmoa infix >>= >> ::= ==

fun qq s = "'" ^ s ^ "'"

fun tstM s ty e f =
    tstopt s (fn () => let val M = f()
                           val p = runM ty M
                           val v = eval p Uv
                           val result = ppV v
		       in if result = e then NONE
			  else SOME (qq result ^ " differs from the expected value " ^ qq e)
		       end)

fun tstF s (ta,tb) f a e =
    tstopt s (fn () => let val p = runF (ta,tb) f
                           val v = eval p a
                           val result = ppV v
		       in if result = e then NONE
			  else SOME (qq result ^ " differs from the expected value " ^ qq e)
		       end)

fun tstmv s ty e f = tstM s ty e (ret o f)

val zi = zilde Int
val _ = tstmv "dim_zilde" Type.Int "1" (fn () => dim zi)
val _ = tstmv "dim_scl" Type.Int "0" (fn () => dim(scl (I 28)))
val _ = tstmv "dim_vec0" Type.Int "1" (fn () => dim(vec (empty Int)))
val _ = tstmv "dim_vec1" Type.Int "0" (fn () => dim(vec (single (I 3))))
val _ = tstmv "dim_vec2" Type.Int "1" (fn () => dim(vec (fromList[I 3,I 2])))

val _ = tstmv "siz_zilde" Type.Int "0" (fn () => siz zi)
val _ = tstmv "siz_scl" Type.Int "1" (fn () => siz(scl (I 45)))
val _ = tstmv "siz_vec0" Type.Int "0" (fn () => siz(vec (empty Int)))
val _ = tstmv "siz_vec1" Type.Int "1" (fn () => siz(vec (single(I 3))))
val _ = tstmv "siz_vec2" Type.Int "2" (fn () => siz(vec (fromList[I 3,I 2])))

fun sumv (v : Int v) : INT M = foldl(ret o op +) (I 0) v

val _ = tstmv "shape_zilde_len" Type.Int "1" (fn () => length(shape zi))
val _ = tstM "shape_zilde_sum" Type.Int "0" (fn () => 
                                                let val mv = zi
                                                    val v = shape mv
                                                in sumv v
                                                end)
val _ = tstmv "shape_scl" Type.Int "0" (fn () => length(shape(scl (I 45))))
val _ = tstmv "shape_vec0_len" Type.Int "1" (fn () => length(shape(vec (empty Int))))
val _ = tstM "shape_vec0_sum" Type.Int "0" (fn () => sumv(shape(vec (empty Int))))
val _ = tstmv "shape_vec1" Type.Int "0" (fn () => length(shape(vec (single(I 3)))))
val _ = tstmv "shape_vec2_len" Type.Int "1" (fn () => length(shape(vec (fromList[I 3,I 2]))))
val _ = tstM "shape_vec2_sum" Type.Int "2" (fn () => sumv(shape(vec (fromList[I 3,I 2]))))

infix === =-=
fun a === b = meq (op ==) a b
fun a =-= b = eq (op ==) a b

fun tstT s f = tstM s Type.Bool "true" f

val A = vec (fromList[I 1,I 2,I 3,I 4])
val _ = tstT "rav1" (fn () => rav(scl(I 34)) === scl(I 34))
val _ = tstT "rav2" (fn () => rav zi === zi)
val _ = tstT "rav3" (fn () => rav A === A)

val v22 = fromList[I 2,I 2]
val v5 = single(I 5)
val _ = tstT "reshape0a" (fn () => reshape v5 A >>= (fn A' => reshape v22 A' >>= (fn A'' => A'' === zi)))

val _ = tstT "reshape0b" (fn () => reshape v22 zi >>= (fn z => z === zi))

val _ = tstT "reshape1a" (fn () => reshape v22 A >>= (fn A' => shape A' =-= v22))

val _ = tstT "reshape1b" (fn () => reshape v22 A >>= (fn A' => rav(A') === A))

val _ = tstT "reshape2"  (fn () => reshape (single(siz A)) A >>= (fn A' => rav A === A'))

fun all s f l =
    let fun a n nil = ()
          | a n (x::xs) =
            (tstT (s ^ "." ^ Int.toString n) (fn () => f x);
             a (Int.+(n,1)) xs)
    in a 0 l
    end

val _ = all "reshape3" (fn X => reshape (shape X) (rav X) >>= (fn X' => X' === X)) [A,zi]

val _ = tstT "zilde" (fn () => zi === zi)

val _ = tstT "index1a" (fn () => index (single(I 1)) A >>= (fn v => v === scl (I 2)))

val _ = tstT "index1b" (fn () => index (single(I 0)) A >>= (fn v => v === scl (I 1)))

val _ = tstT "index1c" (fn () => index (single(I 4)) A >>= (fn v => v === zi))

val _ = tstT "index2" (fn () => reshape v22 A >>= (fn A' => index (single(I 1)) A' >>= (fn v => v === vec (fromList[I 3,I 4]))))

val v10 = fromList[I 1,I 0]
val _ = tstT "index3" (fn () => reshape v22 A >>= (fn A' => index v10 A' >>= (fn v => v === scl (I 3))))

val v1000 = fromList[I 1,I 0,I 0,I 0]
val _ = tstT "index4" (fn () => reshape v22 A >>= (fn A' => index v1000 A' >>= (fn v => v === scl (I 3))))
val _ = tstT "index5" (fn () => index (single(I 1)) zi >>= (fn v => v === zi))
val _ = tstT "index6" (fn () => index (single(I 0)) (scl (I 5)) >>= (fn v => v === scl (I 5)))
val _ = tstT "index7" (fn () => index (single(I 1)) (scl (I 5)) >>= (fn v => v === zi))

val v123 = fromList[I 1,I 2,I 3]
val _ = tstT "iota1" (fn () => iota (I 3) === vec v123)
val _ = tstT "iota2" (fn () => iota (I 1) === scl (I 1))
val _ = tstT "iota3" (fn () => iota (I 0) === zi)

val v2345 = fromList[I 2,I 3,I 4,I 5]
val _ = tstT "map0" (fn () => mmap (fn x => x + (I 1)) zi === zi)
val _ = tstT "map1" (fn () => mmap (fn x => x + (I 1)) A === vec v2345)
val _ = tstT "map2" (fn () => reshape v22 A >>= (fn A' =>
                              reshape v22 (vec v2345) >>= (fn A'' =>
                              mmap (fn x => x + (I 1)) A' === A'')))

(*
val _ = tst "fmap0" (fn () => A.fmap (A.zilde()) 4 == A.zilde())
val _ = tst "fmap1" (fn () => A.fmap (A.vec [fn x => x+1, fn x => x+2]) 4 == A.vec[5,6])
*)

fun curry f x y = f(x,y)
val _ = tstT "red1" (fn () => red (ret o op +) (I 0) A >>= (fn r => ret(r == (I 10))))
val _ = tstT "red2" (fn () => red (ret o op +) (I 0) zi >>= (fn r => ret(r == (I 0))))

val v46913 = fromList[I 4,I 6,I 9,I 13]
val _ = tstT "scan1" (fn () => scan (op +) (I 3) A >>= (fn v => 
                               v === vec v46913))
val _ = tstT "scan2" (fn () => reshape v22 A >>= (fn A' =>
                               scan (op +) (I 3) A' >>= (fn v =>
                               reshape v22 (vec v46913) >>= (fn v' =>
                               v === v'))))

fun outplus x y = out Int (op +) x y
local
    val a = I 10
    val b = I 20
    val A = vec(fromList[a,b])
    val B = vec(fromList[I 1,I 2])
    val r = fromList[a + I 1,a + I 2,b + I 1,b + I 2]
in
val _ = tstT "out1_shape" (fn () => 
                        outplus A B >>= (fn X =>
                        reshape v22 (vec r) >>= (fn Y =>
                        (shape X) =-= (shape Y))))
val _ = tstT "out1" (fn () => 
                        outplus A B >>= (fn X =>
                        reshape v22 (vec r) >>= (fn Y =>
                        X === Y)))

val _ = tstT "out2 - Prop10.2a" (fn () => outplus A zi >>= (fn v => v === zi))
val _ = tstT "out3 - Prop10.2b" (fn () => outplus zi B >>= (fn v => v === zi))
val _ = tstT "out4" (fn () => outplus zi zi >>= (fn v => v === zi))
val _ = tstT "out5 - Prop10.3a" (fn () => outplus (scl a) A >>= (fn v => v === mmap (fn x => x + a) A))
val _ = tstT "out6 - Prop10.3b" (fn () => outplus A (scl a) >>= (fn v => v === mmap (fn x => x+a) A))

infix **
val op ** = fn (a,b) => outplus a b
val C = vec (fromList[I 150,I 170])
val _ = tstT "out7 - Prop10.5" (fn () => A ** B >>= (fn x => x**C >>= (fn v1 =>
                                         B ** C >>= (fn x => A**x >>= (fn v2 => v1 === v2)))))
val _ = tstT "sum1" (fn () => sum Int (op +) A B >>= (fn v => v === vec(fromList([I 11,I 22]))))
val _ = tstT "sum2" (fn () => sum Int (op +) A (vec(fromList[I 1,I 2,I 3])) >>= (fn v => v === zi))
end

(*
val _ = tsta "stk1" (fn () => (A.stk(A.scl 5)(A.scl 8),A.vec[5,8]))
val _ = tsta "stk2" (fn () => (A.stk(A.scl 5)(A.vec[6,7]),A.vec[5,6,7]))
val _ = tsta "stk3" (fn () => (A.stk(A.vec[5,6])(A.scl 7),A.vec[5,6,7]))
val _ = tsta "stk4" (fn () => (A.stk(A.vec[5,6])(A.vec[7,8]),A.reshape [2,2] (A.vec[5,6,7,8])))
val _ = tsta "stk5" (fn () => (A.stk(A.stk(A.vec[1,2])(A.vec[3,4]))(A.stk(A.vec[5,6])(A.vec[7,8])),A.reshape [2,2,2] (A.iota 8)))
val _ = tsta "stk6 - Prop12.1a" (fn () => (A.stk A (A.zilde()),A))
val _ = tsta "stk7 - Prop12.1b" (fn () => (A.stk (A.zilde()) A,A))

val _ = tst "pr0" (fn () => pr (A.zilde()) = "[]")
val _ = tst "pr1" (fn () => pr A = "[1,2,3,4]")
val _ = tst "pr2" (fn () => pr (A.reshape [2,2] A) = "[[1,2],[3,4]]")
val _ = tst "pr3" (fn () => pr (A.reshape [2,0] (A.zilde())) = (*"[[],[]]"*) "[]")
val _ = tst "pr4" (fn () => pr (A.reshape [0,2] (A.zilde())) = "[]")

(*val B = A.vec *)
*)

val _ = tstF "iota5" (Type.Int, Type.Int) (fn v => ret(siz(iota(v + I 2)))) (Iv 5) "7"
val _ = tstF "red5" (Type.Int, Type.Int) (fn v => red (ret o op +) (I 0) (iota v)) (Iv 5) "15"

val () = finish()                           

end
