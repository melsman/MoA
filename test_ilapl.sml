structure TestILapl = struct

open UTest

val () = start "test_ilapl.sml" "structure ILapl (ilapl.sml)"

open ILapl infix >>= >> ::= ==

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
val _ = tstmv "dim_zilde" Int "1" (fn () => dim zi)
val _ = tstmv "dim_scl" Int "0" (fn () => dim(scl Int (I 28)))
val _ = tstmv "dim_vec0" Int "1" (fn () => dim(vec (empty Int)))
val _ = tstmv "dim_vec1" Int "1" (fn () => dim(vec (single Int (I 3))))
val _ = tstmv "dim_vec2" Int "1" (fn () => dim(vec (fromList Int [I 3,I 2])))

val _ = tstmv "siz_zilde" Int "0" (fn () => siz zi)
val _ = tstmv "siz_scl" Int "1" (fn () => siz(scl Int (I 45)))
val _ = tstmv "siz_vec0" Int "0" (fn () => siz(vec (empty Int)))
val _ = tstmv "siz_vec1" Int "1" (fn () => siz(vec (single Int (I 3))))
val _ = tstmv "siz_vec2" Int "2" (fn () => siz(vec (fromList Int [I 3,I 2])))

fun sumv (v : Int Num v) : INT M = foldl(ret o op +) (I 0) v

val _ = tstmv "shape_zilde_len" Int "1" (fn () => length(shape zi))
val _ = tstM "shape_zilde_sum" Int "0" (fn () => 
                                                let val mv = zi
                                                    val v = shape mv
                                                in sumv v
                                                end)
val _ = tstmv "shape_scl" Int "0" (fn () => length(shape(scl Int (I 45))))
val _ = tstmv "shape_vec0_len" Int "1" (fn () => length(shape(vec (empty Int))))
val _ = tstM "shape_vec0_sum" Int "0" (fn () => sumv(shape(vec (empty Int))))
val _ = tstmv "shape_vec1" Int "1" (fn () => length(shape(vec (single Int (I 3)))))
val _ = tstmv "shape_vec2_len" Int "1" (fn () => length(shape(vec (fromList Int [I 3,I 2]))))
val _ = tstM "shape_vec2_sum" Int "2" (fn () => sumv(shape(vec (fromList Int [I 3,I 2]))))

infix === =-=
fun a === b = meq (op ==) a b
fun a =-= b = eq (op ==) a b

fun tstT s f = tstM s Bool "true" f

val A = vec (fromList Int [I 1,I 2,I 3,I 4])
val _ = tstT "rav1" (fn () => rav(scl Int (I 34)) === vec(fromList Int [I 34]))
val _ = tstT "rav2" (fn () => rav zi === zi)
val _ = tstT "rav3" (fn () => rav A === A)

val v22 = fromList Int [I 2,I 2]
val v5 = single Int (I 5)
val _ = tstT "reshape0a" (fn () => reshape v5 A >>= (fn A' => reshape v22 A' >>= (fn A'' => rav A'' === A)))

val _ = tstT "reshape0b" (fn () => reshape v22 zi >>= (fn z => 
                                                          reshape v22 (vec(fromList Int [I 0,I 0,I 0,I 0])) >>= (fn z1 =>
                                                              z === z1)))

val _ = tstT "reshape1a" (fn () => reshape v22 A >>= (fn A' => shape A' =-= v22))

val _ = tstT "reshape1b" (fn () => reshape v22 A >>= (fn A' => rav A' === A))

val _ = tstT "reshape2"  (fn () => reshape (single Int (siz A)) A >>= (fn A' => rav A === A'))

fun all s f l =
    let fun a n nil = ()
          | a n (x::xs) =
            (tstT (s ^ "." ^ Int.toString n) (fn () => f x);
             a (Int.+(n,1)) xs)
    in a 0 l
    end

val _ = all "reshape3" (fn X => reshape (shape X) (rav X) >>= (fn X' => X' === X)) [A,zi]

val _ = tstT "zilde" (fn () => zi === zi)

(*
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
*)

val v123 = fromList Int [I 1,I 2,I 3]
val _ = tstT "iota1" (fn () => iota (I 3) === vec v123)
val _ = tstT "iota2" (fn () => iota (I 1) === vec(fromList Int [I 1]))
val _ = tstT "iota3" (fn () => iota (I 0) === zi)

val v2345 = fromList Int [I 2,I 3,I 4,I 5]
val _ = tstT "each0" (fn () => each Int  (fn x => ret(x + (I 1))) zi === zi)
val _ = tstT "each1" (fn () => each Int (fn x => ret(x + (I 1))) A === vec v2345)
val _ = tstT "each2" (fn () => reshape v22 A >>= (fn A' =>
                               reshape v22 (vec v2345) >>= (fn A'' =>
                               each Int (fn x => ret(x + (I 1))) A' === A'')))

(*
val _ = tst "fmap0" (fn () => A.fmap (A.zilde()) 4 == A.zilde())
val _ = tst "fmap1" (fn () => A.fmap (A.vec [fn x => x+1, fn x => x+2]) 4 == A.vec[5,6])
*)

fun curry f x y = f(x,y)
val _ = tstT "red1" (fn () => red (ret o op +) (I 0) A >>= (fn r => ret(r == (I 10))))
val _ = tstT "red2" (fn () => red (ret o op +) (I 0) zi >>= (fn r => ret(r == (I 0))))

val v46913 = fromList Int [I 4,I 6,I 9,I 13]
val _ = tstT "scan1" (fn () => scan (op +) (I 3) A >>= (fn v => 
                               v === vec v46913))
val _ = tstT "scan2" (fn () => reshape v22 A >>= (fn A' =>
                               scan (op +) (I 3) A' >>= (fn v =>
                               reshape v22 (vec v46913) >>= (fn v' =>
                               v === v'))))

(*
fun outplus x y = out Int (op +) x y
*)
local
    val a = I 10
    val b = I 20
    val A = vec(fromList Int [a,b])
    val B = vec(fromList Int [I 1,I 2])
    val r = fromList Int [a + I 1,a + I 2,b + I 1,b + I 2]
in
(*
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
val _ = tstT "out5 - Prop10.3a" (fn () => outplus (scl a) A >>= (fn v => v === each (fn x => x + a) A))
val _ = tstT "out6 - Prop10.3b" (fn () => outplus A (scl a) >>= (fn v => v === each (fn x => x+a) A))

infix **
val op ** = fn (a,b) => outplus a b
val C = vec (fromList[I 150,I 170])
val _ = tstT "out7 - Prop10.5" (fn () => A ** B >>= (fn x => x**C >>= (fn v1 =>
                                         B ** C >>= (fn x => A**x >>= (fn v2 => v1 === v2)))))
*)
val _ = tstT "sum1" (fn () => sum Int (ret o op +) A B >>= (fn v => v === vec(fromList Int [I 11,I 22])))
val _ = tstT "sum2" (fn () => sum Int (ret o op +) A (vec(fromList Int [I 1,I 2,I 3])) >>= (fn v => v === zi))
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

val _ = tstF "iota5" (Int, Int) (fn v => ret(siz(iota(v + I 2)))) (Iv 5) "7"
val _ = tstF "red5" (Int, Int) (fn v => red (ret o op +) (I 0) (iota v)) (Iv 5) "15"

(* catenate *)

val _ = tstF "catenate1" (Int, Int) (fn v => 
                                        let val a = iota(v + I 2)
                                            val b = iota(v + I 3)
                                        in catenate a b >>= (fn c => ret(siz c))
                                        end) (Iv 5) "15"

val _ = tstF "catenate2" (Int, Int) (fn v => 
                                        let val a = iota(v + I 2)
                                            val b = iota(v + I 3)
                                        in catenate a b >>= (fn c => red (ret o op +) (I 0) c)
                                        end) (Iv 5) "64"

val _ = tstF "catenate3" (Int, Int) (fn v => 
                                        let val a = iota(v + I 3)
                                            val b = iota(v + I 1)
                                        in reshape (fromList Int [I 4,I 2]) a >>= (fn a1 =>
                                           reshape (fromList Int [I 3,I 2]) b >>= (fn b1 =>
                                           catenate a1 b1 >>= (fn c =>
                                           red (ret o op +) (I 0) c)))
                                        end) (Iv 5) "57"
(*
val _ = tstF "catenate4" (Type.Int, Type.Int) (fn v => 
                                                  let val a = iota(v + I 3)
                                                      val b = iota(v + I 2)  (* mismatch: so the whole thing should reduce to zilde *)
                                                  in reshape (fromList[I 4,I 2]) a >>= (fn a1 =>
                                                     reshape (fromList[I 3,I 2]) b >>= (fn b1 =>
                                                     catenate a1 b1 >>= (fn c =>
                                                     red (ret o op +) (I 0) c)))
                                                  end) (Iv 5) "0"
*)
(*
val _ = tstF "catenate5" (Type.Int, Type.Int) (fn v =>
                                                  let val a = iota(v + I 3)
                                                      val b = iota(v + I 1)
                                                  in reshape (fromList[I 4,I 2]) a >>= (fn a1 =>
                                                     reshape (fromList[I 3,I 2]) b >>= (fn b1 =>
                                                     catenate a1 b1 >>= (fn c =>
                                                     index (fromList[I 5, I 1]) c >>= (fn c' =>
                                                     red (ret o op +) (I 0) c'))))
                                                  end) (Iv 5) "4"
*)

(*
  specification:
  [0.5,1.5,....,99.5,...,0.5,....99.5]   <--- 200x100000 elements total
  should add up to 35594874.59054118
*)

val rotate = fn x => fn y => ret (rotate x y)

val _ = tstF "rrotate1" (Int, Double) 
             (fn v => 
                 let val a = iota (v + I 2)
                     val a = each Double (ret o i2d) a
                 in catenate (scl Double (D 0.0)) a >>= (fn a =>
                    rotate (I ~1) a >>= (fn b => 
                    red (ret o op +) (D 0.0) b))
                 end) (Iv 5) "28.0"

val _ = tstF "rrotate2" (Int, Int) 
             (fn v => 
                 let val a = iota (v + I 2)
                     val a = each Double (ret o i2d) a
                 in catenate (scl Double (D 0.0)) a >>= (fn a =>
                    rotate (I ~1) a >>= (fn b => 
                    ret (siz b)))
                 end) (Iv 5) "8"

fun sub1 (x, a) = If(a == D 0.0, x, a - x)

val _ = tstF "rrotate3" (Int, Double) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a
                 in rotate (I ~1) a >>= (fn b =>            (* 5 1 2 3 4 *)
                    red (ret o sub1) (D 0.0) b)
                 end) (Iv 5) "-5.0"

val _ = tstF "rrotate4" (Int, Int) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a
                 in rotate (I ~1) a >>= (fn b =>
                    ret (length(shape b)))
                 end) (Iv 5) "1"

fun diff (SIG (*:n*)) (*:n-1*) =
    rotate (I ~1) SIG >>= (fn r => 
    sum Double (ret o op -) SIG r >>= (fn (r' (*:n*)) =>
    ret (drop (I 1) r'))) (* take (siz r' - I 1) r'))) *)

val _ = tstF "diff0" (Int, Int)
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a
                 in diff a >>= (fn b =>
                    ret (siz b))
                 end) (Iv 5) "4"

val _ = tstF "diff1" (Int, Double) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a
                 in diff a >>= (fn b =>
                    red (ret o sub1) (D 0.0) b)
                 end) (Iv 5) "0.0"

val _ = tstF "diff2" (Int, Double) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a          (* 1 2 3 4 5 *)   (* 1  2  3  4  5 *)
                 in diff a >>= (fn b =>                                    (* 5  1  2  3  4 *)
                    red (ret o op +) (D 0.0) b)                            (*-4  1  1  1  1 *)
                 end) (Iv 5) "4.0"

val _ = tstF "diff3" (Int, Int) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a                 
                 in catenate (scl Double (D 0.0)) a >>= (fn a => 
                    diff a >>= (fn b => ret (siz b)))
                 end) (Iv 5) "5"

fun prodv v = foldl (ret o op *) (I 1) v

val _ = tstF "catenate9" (Int, Int) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a                 
                 in catenate (scl Double (D 0.0)) a >>= (fn a => prodv (shape a))
                 end) (Iv 5) "6"

val _ = tstF "catenate10" (Int, Int) 
             (fn v => 
                 let val a = iota v
                     val a = each Double (ret o i2d) a                 
                 in catenate (scl Double (D 0.0)) a >>= (fn a => ret (length (shape a)))
                 end) (Iv 5) "1"

fun maxsv s v = each Double (fn x => ret(max x s)) v
fun minsv s v = each Double (fn x => ret(min x s)) v
fun prodsv s v = each Double (fn x => ret(x * s)) v
fun addsv s v = each Double (fn x => ret(x + s)) v
fun divv v1 v2 = sum Double (ret o op /) v1 v2

fun signal0 (SIG (*:n*)) =
    catenate (scl Double (D 0.0)) SIG >>= (fn (c (*:n+1*)) =>
    diff c)

fun signal (SIG (*:n*)) =
    catenate (scl Double (D 0.0)) SIG >>= (fn (c (*:n+1*)) =>
    diff c >>= (fn (v (*:n*)) => 
    divv v (addsv (D 0.01) SIG) >>= (fn tmp =>
    ret(maxsv (D ~50.0) (minsv (D 50.0) (prodsv (D 50.0) tmp))))))

fun absplus (x,a) =
    If(x < D 0.0, a - x, a + x)

val _ = tstF "catenate8" (Int, Double)
             (fn n =>
                 let val v = iota (I 2 * n)
                     val v = each Double (ret o i2d) v
                     val v = each Double (fn d => ret(d / D 2.0)) v                             
                 in catenate (scl Double (D 0.0)) v >>= (fn v' => (* v'=[0.0 0.5 1.0 ... 100.0]; sum(v') = 100*100+50 *)
                    red (ret o absplus) (D 0.0) v')
                 end) (Iv 100) "10050.0"

val _ = tstF "signal0" (Int, Int)
             (fn n =>
                 let val v = iota (I 2 * n)
                     val v = each Double (ret o i2d) v
                     val v = each Double (fn d => ret(d / D 2.0)) v                             
                 in signal0 v >>= (fn v' => ret (siz v'))
                 end) (Iv 100) "200"

val _ = tstF "signal" (Int, Double)
             (fn n =>
                 let val n = I 500
                     val v = iota (I 2 * n)
                     infix %
                     val v = each Int (fn x => ret(x % (I 200))) v
                     val v = each Double (ret o i2d) v
                     val v = each Double (fn d => ret(d / D 2.0)) v
                 in mem v >>= (fn v =>
                    signal v >>= (fn v' => red (ret o op +) (D 0.0) v'))
                 end) (Iv (500)) "1210.17620977" 

val () = finish()                           

end
