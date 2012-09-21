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

(*
fun tsta s f =
    tst s (fn () =>
	      let val (a1 : int V.t,a2) = f()
	      in V.eq (op =) (a1, a2)
	      end)
*)

fun prl vs = "[" ^ String.concatWith "," (List.map Int.toString vs) ^ "]"
fun prv v = let val l = V.length v 
            in prl (V.list v) ^ (Int.toString l)
            end

(*
fun tsta s f =
    tstopt s (fn () => let val (a: int V.t,b) = f()
		       in if V.eq (op =) (a,b) then NONE
			  else SOME (prv a ^ " is not equal to " ^ prv b)
		       end)
fun tstl s f =
    tstopt s (fn () => let val (a: int list,b) = f()
		       in if V.eq (op =) (a,b) then NONE
			  else SOME (prl a ^ " is not equal to " ^ prv b)
		       end)

*)

local
  fun tstg (pr,eq) s f =
      tstopt s (fn () => let val (a,b) = f()
		         in if eq (a,b) then NONE
			    else SOME (pr a ^ " is not equal to " ^ pr b)
		         end)
in
  val tstl = tstg (prl, op =)
  val tsta = tstg (prv, V.eq (op=))
end

fun push v = V.concat (V.empty()) v

val a0 = V.tabulate 3 (fn x => x)
val a0 = push a0
val a1 = V.map (fn x => x + 1) a0
val _ = tsta "map1" (fn () => (a1, V.fromList[1,2,3]))

val _ = tsta "concat" (fn () => (V.concat a0 a1, V.fromList[0,1,2,1,2,3]))
val _ = tstl "concat2" (fn () => (V.list(V.concat a0 a1), [0,1,2,1,2,3]))

val _ = tsta "tk0" (fn () => (V.tk 0 a1, V.empty()))
val _ = tsta "tk1" (fn () => (V.tk 2 a1, V.fromList[1,2]))
val _ = tsta "tk2" (fn () => (V.tk 3 a1, a1))
val _ = tsta "dr0" (fn () => (V.dr 0 a1, a1))
val _ = tsta "dr1" (fn () => (V.dr 1 a1, V.fromList[2,3]))
val _ = tstl "dr1a" (fn () => (V.list(V.dr 1 a1), [2,3]))
val _ = tstl "dr1b" (fn () => (V.list(V.dr 1 (V.concat (V.dr 1 a1) a0)), [3,0,1,2]))
val _ = tstl "dr1c" (fn () => (V.foldl (op ::) nil (V.dr 1 (V.concat (V.dr 1 a1) a0)), [2,1,0,3]))
val _ = tstl "dr1'" (fn () => (V.list(V.dr 1 a0), [1,2]))
val _ = tsta "dr2" (fn () => (V.dr 3 a1, V.empty()))
val _ = tsta "dr3" (fn () => (V.dr 1 a0, V.fromList[1,2]))

val _ = tsta "map2" (fn () => (V.map2 (fn x => fn y => x * y) a0 a1, V.fromList[0,2,6]))

val f2 = V.fromList[fn x => x * x, fn x => x + x]
val _ = tsta "fmap" (fn () => (V.fmap f2 5, V.fromList[25,10]))
val _ = tsta "empty" (fn () => (V.empty(), V.fromList[]))
val _ = tsta "single" (fn () => (V.single 8, V.fromList[8]))
val _ = tst "length" (fn () => V.length a0 = 3 andalso V.length(V.empty()) = 0)
val _ = tstl "list" (fn () => (V.list a1, [1,2,3]))
val _ = tst "foldl" (fn () => V.foldl (op +) 0 a1 = 6)
val _ = tstl "foldl1" (fn () => (V.foldl (op ::) [] a1, [3,2,1]))
val _ = tstl "foldr1" (fn () => (V.foldr (op ::) [] a1, [1,2,3]))
val _ = tsta "flatten" (fn () => (V.flatten(V.fromList[a0,a1,V.empty(),a1]), V.fromList[0,1,2,1,2,3,1,2,3]))
val _ = tsta "flatten2" (fn () => (V.flatten(V.fromList[a0,V.map (fn x => x*3) (V.flatten(V.fromList[a1,V.empty(),a1]))]), 
                                   V.fromList[0,1,2,3,6,9,3,6,9]))

val _ = tstl "flatten3" (fn () => 
                           let val v = V.flatten(V.fromList[a0,V.map (fn x => x*3) (V.flatten(V.fromList[a1,V.empty(),a1]))])
                               val l = V.list v
                           in (l, [0,1,2,3,6,9,3,6,9])
                           end)
val _ = tstl "flatten4" (fn () => 
                           let val v = V.flatten(V.fromList[a0,a1])
                               val l = V.list v
                           in (l, [0,1,2,1,2,3])
                           end)

val _ = tsta "rev0" (fn () => (V.rev (V.empty()), V.empty()))
val _ = tsta "rev1" (fn () => (V.rev (V.single 3), V.single 3))
val _ = tsta "rev2" (fn () => (V.rev a1, V.fromList[3,2,1]))
val _ = tsta "rev3" (fn () => (V.rev(V.concat a0 (V.concat(V.rev a1)(V.rev a1))), V.fromList[1,2,3,1,2,3,2,1,0]))

val () = finish()

end

local
    structure X = Test(structure V = ListVec val name = "ListVec")
    structure Y = Test(structure V = Fvec val name = "Fvec")
    structure Z = Test(structure V = PPvec val name = "PPvec")
in
end
