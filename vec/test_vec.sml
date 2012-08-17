(* Simple unit tests for the vector library *)

functor Test(structure V : VEC
             val name : string) = struct

open UTest

val () = start "test_vec.sml" ("structure " ^ name)

local
  val is_debug = false
in
fun debug s = if is_debug then print s
              else ()
end

fun tsta s f =
    tst s (fn () =>
	      let val (a1 : int V.t,a2) = f()
	      in V.eq (op =) (a1, a2)
	      end)

val a0 = V.tabulate 3 (fn x => x)
val a1 = V.map (fn x => x + 1) a0
val _ = tsta "map1" (fn () => (a1, V.fromList[1,2,3]))

val _ = tsta "concat" (fn () => (V.concat a0 a1, V.fromList[0,1,2,1,2,3]))

val _ = tsta "tk0" (fn () => (V.tk 0 a1, V.empty()))
val _ = tsta "tk1" (fn () => (V.tk 2 a1, V.fromList[1,2]))
val _ = tsta "tk2" (fn () => (V.tk 3 a1, a1))
val _ = tsta "dr0" (fn () => (V.dr 0 a1, a1))
val _ = tsta "dr1" (fn () => (V.dr 1 a1, V.fromList[2,3]))
val _ = tsta "dr2" (fn () => (V.dr 3 a1, V.empty()))

val _ = tsta "map2" (fn () => (V.map2 (fn x => fn y => x * y) a0 a1, V.fromList[0,2,6]))

val f2 = V.fromList[fn x => x * x, fn x => x + x]
val _ = tsta "fmap" (fn () => (V.fmap f2 5, V.fromList[25,10]))
val _ = tsta "empty" (fn () => (V.empty(), V.fromList[]))
val _ = tsta "single" (fn () => (V.single 8, V.fromList[8]))
val _ = tst "length" (fn () => V.length a0 = 3 andalso V.length(V.empty()) = 0)
val _ = tst "list" (fn () => V.list a1 = [1,2,3])
val _ = tst "foldl" (fn () => V.foldl (op +) 0 a1 = 6)
val _ = tsta "flatten" (fn () => (V.flatten(V.fromList[a0,a1,V.empty(),a1]), V.fromList[0,1,2,1,2,3,1,2,3]))

val () = finish()

end

local
    structure X = Test(structure V = ListVec val name = "ListVec")
    structure Y = Test(structure V = Fvec val name = "Fvec")
in
end
