(* Pull vectors - version which allows map fusion with computations *)

structure ILvecm :> ILVECM = struct

fun die s = raise Fail ("ILvecm." ^ s)

open IL

type 'a M = 'a * Program.p
type 'a e = 'a Exp.e
type 'a v = int e * (int e -> 'a e M)

local open Exp Program infix >> ::= in
  infix >>=
  fun (v,p) >>= f = let val (v',p') = f v in (v',p >> p') end
  fun ret v = (v, emp)
  fun runM (e,p) n = p >> n := e

  fun map f (n,g) = (n, fn i => g i >>= f)
  fun map2 f (n1,f1) (n2,f2) = 
      (min n1 n2, fn i => f1 i >>= (fn x1 =>
                          f2 i >>= (fn x2 => 
                          f x1 x2))
      )
  fun rev (n,g) = (n, fn i => g(n - i - I 1))
  fun tabulate n f = (n,f)
  fun emp _ = die "emp.impossible"
  fun empty () = (I 0, emp)
  fun tk n (a as (m,g)) = (min n m, g)
  fun dr n (m,g) = (max (m-n) (I 0), fn i => g(i+n))
  fun length (n,_) = n

  type prog = int Name.t -> p
  fun sum (n,f) name =
      name := I 0 >>
      For(n, fn i => #2(f(V i) >>= (fn v => (V name, name := v + V name))))

  fun eval (p: prog) : Value =
      let val name = Name.new Type.INT
          val program = p name
          val () = print (ILUtil.ppProgram program ^ "\n")
          val env0 = ILUtil.emptyEnv
          val env = ILUtil.evalProgram env0 program        
      in case ILUtil.lookup env name of
           SOME v => v
         | NONE => die ("Error finding " ^ Name.pr name ^ " in result environment for evaluation of\n" ^
                        ILUtil.ppProgram program)
      end

  fun memoize (n,f) =
      let val name = Name.new (Type.ARRAY (Type.VAR()))
          val p = name := Alloc n >>
                  For(n, fn i => #2(f(V i) >>= (fn v => (V name,(name,V i) ::= v))))
      in ((n, fn i => ret(Subs(name,i))), p)
      end

  fun foldl f e (n,g) =
      let val a = Name.new (Type.VAR())
          fun body i =
              runM (g(V i) >>= (fn v => f(v,V a))) a
          val p = a := e >>
                  For(n, body)
      in (V a,p)
      end

  fun foldr f e (n,g) =
      let val a = Name.new (Type.VAR())
          val n0 = Name.new (Type.INT)
          fun body i =
              runM (g(V n0 - V i) >>= (fn v => f(v,V a))) a
          val p = n0 := n - I 1 >>
                  a := e >>
                  For(n, body)
      in (V a,p)
      end

  fun concat (n1,f1) (n2,f2) =
      let val a = Name.new (Type.VAR())
      in (n1+n2, fn i => (V a, Ifp(i < n1, runM (f1 i) a, runM (f2(i-n1)) a)))
      end

(*  fun flatten v = foldl (fn (v,a) => concat a v) (empty()) v *)

(*
  fun flatten v =
      let val m = foldl (fn (a,l) => length a + l) (I 0) v >>= (fn len =>
	          (foldr (fn ((n,f),g) =>
		          fn i => If(i < n, f i, g (i-n))) emp v >>= (fn f =>
      in (len,f)
      end
*)
end

(*

fun list a = foldr (op ::) nil a
	
fun fmap a v = map (fn f => f v) a
fun single x = fromList [x]

fun eq beq (a1,a2) =
    length a1 = length a2 andalso
    (foldl(fn (x,a) => x andalso a) true 
	  (map2 (fn x => fn y => beq(x,y)) a1 a2))

fun concat (n1,f1) (n2,f2) =
    (n1+n2, fn i => if i < n1 then f1 i else f2 (i-n1))

fun flatten v =
    let val len = foldl (fn (a,l) => length a + l) 0 v
	val f = foldr (fn ((n,f),g) =>
		       fn i => if i < n then f i else g (i-n)) emp v
    in (len,f)
    end

fun sub ((n,f),i) =
    if i > n-1 orelse i < 0 then raise Subscript
    else f i

fun memoize (n,f) =
    let val v = Vector.tabulate(n,f)
    in (n, fn i => Unsafe.Vector.sub(v,i))
    end

*)
end
