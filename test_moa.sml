(* Simple unit tests for the APL operations *)

local
  val is_debug = false
in
fun debug s = if is_debug then print s
              else ()
end

open UTest

val () = UTest.start "test_apl.sml" "structure Apl"

structure A = Apl

val pr = A.pp (Int.toString)
fun tsta s f =
    tstopt s (fn () => let val (a: int A.t,b) = f()
		       in if A.eq (op =) (a,b) then NONE
			  else SOME (pr a ^ " is not equal to " ^ pr b)
		       end)

val _ = tst "dim_zilde" (fn () => A.dim(A.zilde()) = 1)
val _ = tst "dim_scl" (fn () => A.dim(A.scl 28) = 0)
val _ = tst "dim_vec0" (fn () => A.dim(A.vec []) = 1)
val _ = tst "dim_vec1" (fn () => A.dim(A.vec [3]) = 0)
val _ = tst "dim_vec2" (fn () => A.dim(A.vec [3,2]) = 1)

val _ = tst "siz_zilde" (fn () => A.siz(A.zilde()) = 0)
val _ = tst "siz_scl" (fn () => A.siz(A.scl 45) = 1)
val _ = tst "siz_vec0" (fn () => A.siz(A.vec []) = 0)
val _ = tst "siz_vec1" (fn () => A.siz(A.vec [3]) = 1)
val _ = tst "siz_vec2" (fn () => A.siz(A.vec [3,2]) = 2)

val _ = tst "shape_zilde" (fn () => A.shape(A.zilde()) = [0])
val _ = tst "shape_scl" (fn () => A.shape(A.scl 45) = [])
val _ = tst "shape_vec0" (fn () => A.shape(A.vec []) = [0])
val _ = tst "shape_vec1" (fn () => A.shape(A.vec [3]) = [])
val _ = tst "shape_vec2" (fn () => A.shape(A.vec [3,2]) = [2])

infix ==
fun a == (b:int A.t) = 
    (debug(pr a ^ " == " ^ pr b ^ "\n");
     A.eq (op =) (a, b))

val _ = tst "rav1" (fn () => A.rav(A.scl 34) == A.scl 34)
val _ = tst "rav2" (fn () => A.rav(A.zilde()) == A.zilde())

val A = A.vec [1,2,3,4]

val _ = tst "reshape0" (fn () => A.reshape [2,2] (A.reshape [5] A) == A.zilde() andalso 
				 A.reshape [2,2] (A.zilde()) == A.zilde())
val _ = tst "reshape1" (fn () => let val r = A.reshape [2,2] A 
				 in A.shape r = [2,2] andalso A.rav r == A
				 end)
val _ = tst "reshape2" (fn () => A.rav A == A.reshape [A.siz A] A)
val _ = all "reshape3" (fn X => A.reshape (A.shape X) (A.rav X) == X) [A,A.zilde()]

val _ = tst "index1" (fn () => A.index [1] A == A.scl 2 andalso
			       A.index [0] A == A.scl 1 andalso A.index [4] A == A.vec [])
val _ = tst "index2" (fn () => A.index [1] (A.reshape [2,2] A) == A.vec [3,4])
val _ = tst "index3" (fn () => A.index [1,0] (A.reshape [2,2] A) == A.scl 3)
val _ = tst "index4" (fn () => A.index [1,0,0,0] (A.reshape [2,2] A) == A.scl 3)
val _ = tst "index5" (fn () => A.index [1] (A.zilde()) == A.zilde())
val _ = tst "index6" (fn () => A.index [0] (A.scl 5) == A.scl 5)
val _ = tst "index7" (fn () => A.index [1] (A.scl 5) == A.zilde())

val _ = tst "iota1" (fn () => A.iota 3 == A.vec [1,2,3])
val _ = tst "iota2" (fn () => A.iota 1 == A.scl 1)
val _ = tst "iota3" (fn () => A.iota 0 == A.zilde())

val _ = tst "map0" (fn () => A.map (fn x => x+1) (A.zilde()) == A.zilde())
val _ = tst "map1" (fn () => A.map (fn x => x+1) A == A.vec[2,3,4,5])
val _ = tst "map2" (fn () => A.map (fn x => x+1) (A.reshape [2,2] A) == A.reshape [2,2] (A.vec[2,3,4,5]))
val _ = tst "fmap0" (fn () => A.fmap (A.zilde()) 4 == A.zilde())
val _ = tst "fmap1" (fn () => A.fmap (A.vec [fn x => x+1, fn x => x+2]) 4 == A.vec[5,6])

fun curry f x y = f(x,y)
val _ = tst "red1" (fn () => A.red (curry op +) 0 A = 10)
val _ = tst "red2" (fn () => A.red (curry op +) 0 (A.zilde()) = 0)

val _ = tst "scan1" (fn () => A.scan (curry op +) 3 A == A.vec [4,6,9,13])
val _ = tst "scan2" (fn () => A.scan (curry op +) 3 (A.reshape [2,2] A) == A.reshape [2,2] (A.vec [4,6,9,13]))

local
    val a = 10
    val b = 20
    val A = A.vec[a,b]
    val B = A.vec[1,2]
    val g = curry op +
in
val _ = tst "out1" (fn () => A.out g A B == A.reshape [2,2] (A.vec[a+1,a+2,b+1,b+2]))
val _ = tsta "out2 - Prop10.2a" (fn () => (A.out g A (A.zilde()), A.zilde()))
val _ = tsta "out3 - Prop10.2b" (fn () => (A.out g (A.zilde()) B, A.zilde()))
val _ = tsta "out4" (fn () => (A.out g (A.zilde()) (A.zilde()), A.zilde()))
val _ = tsta "out5 - Prop10.3a" (fn () => (A.out g (A.scl a) A, A.map (g a) A))
val _ = tsta "out6 - Prop10.3b" (fn () => (A.out g A (A.scl a), A.fmap (A.map g A) a))
infix **
val op ** = fn (a,b) => A.out g a b
val C = A.vec [150,170]
val _ = tsta "out7 - Prop10.5" (fn () => ((A**B)**C, A**(B**C)))

val _ = tsta "sum1" (fn () => (A.sum g A B, A.vec [11,22]))
val _ = tsta "sum2" (fn () => (A.sum g A (A.vec [1,2,3]), A.zilde()))
end

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
val _ = tst "pr3" (fn () => pr (A.reshape [2,0] (A.zilde())) = "[[],[]]")
val _ = tst "pr4" (fn () => pr (A.reshape [0,2] (A.zilde())) = "[]")

(*val B = A.vec *)

val () = finish()                           

