(* Pull vectors *)

structure IL = IL(struct type 'a t = unit end)
structure Program = Program(IL)
structure ILUtil = ILUtil(structure IL = IL
                          structure Program = Program)

structure ILvec : ILVEC = struct

type Value = IL.Value
structure P = Program

fun die s = raise Fail ("ILvec2." ^ s)

type Int = unit
type Bool = unit
type 'a Vec = unit

type 'a T = string
val Int = "Int"
val Bool = "Bool"
fun Vec s = s ^ "[]"
 
datatype t0 = E of P.e
            | V of P.e * (P.e -> t0)

type 'a t = t0
type 'a v = t0

type INT     = Int t
type BOOL    = Bool t

val I : int -> INT = E o P.I
val B : bool -> BOOL = E o P.B

fun binop opr (E t1,E t2) = E(opr(t1,t2))
  | binop _ _ = die "binop: expecting expressions"

fun curry f x y = f(x,y)
fun uncurry f (x,y) = f x y

local open P
in
  fun map f (V(n,g)) = V(n, f o g)
    | map f _ = die "map: expecting vector"

  fun map2 f (V(n1,f1)) (V(n2,f2)) = V(min n1 n2, fn i => f(f1 i)(f2 i))
    | map2 _ _ _ = die "map2: expecting vectors"

  fun rev (V(n,g)) = V(n, fn i => g(n - i - I 1))
    | rev _ = die "rev: expecting vector"

  fun tabulate (E n) f = V(n,f o E)
    | tabulate _ _ = die "tabulate: expecting expression"

  fun empty _ = V(I 0, fn _ => die "empty: impossible")

  fun single t = V(I 1, fn _ => t)

  fun tk (E n) (V(m,g)) = V(min n m, g)
    | tk (V _) _ = die "tk: expecting expression"
    | tk _ _ = die "tk: expecting vector"

  fun dr (E n) (V(m,g)) = V(max (m-n) (I 0), fn i => g(i+n))
    | dr (V _) _ = die "dr: expecting expression"
    | dr _ _ = die "dr: expecting vector"

  fun length (V(n,_)) = E n
    | length _ = die "length: expecting vector"
end

val op +  : INT * INT -> INT = binop P.+
val op -  : INT * INT -> INT = binop P.-
val op *  : INT * INT -> INT = binop P.*
val op <  : INT * INT -> BOOL = binop P.<
val op <= : INT * INT -> BOOL = binop P.<=
val op == : INT * INT -> BOOL = binop P.==
val max   : INT -> INT -> INT = curry(binop(uncurry P.max))
val min   : INT -> INT -> INT = curry(binop(uncurry P.min))

type prog = Name.t -> P.p

fun eval (p: prog) : IL.Value =
    let val name = Name.new ()
        val program = p name
        val () = print (ILUtil.ppProgram program ^ "\n")
        val env0 = ILUtil.emptyEnv
        val env = ILUtil.evalProgram env0 program        
    in case ILUtil.lookup env name of
         SOME v => v
       | NONE => die ("Error finding " ^ Name.pr name ^ " in result environment for evaluation of\n" ^
                      ILUtil.ppProgram program)
    end

type 'a M = 'a * P.p
infix >>= >> ::=
val op >> = P.>>
val op := = P.:=
fun (v,p) >>= f = let val (v',p') = f v in (v',p >> p') end
fun ret v = (v, P.emp)
fun runM (E e,p) n = p >> n := e
  | runM _ _ = die "runM: expecting expression"

fun memoize (V(n,f)) =
    let open P
        val name = Name.new ()
        val f = fn i => case f i of E e => e | V _ => die "memoize"
        val p = name := Alloc n >>
                For(n, fn i => (name,$ i) ::= f($ i))
    in (V(n, fn i => E(Subs(name,i))), p)
    end
  | memoize _ = die "memoize: expecting vector"

fun foldl f (E e) (V(n,g)) =
    let open P
        val a = Name.new ()
        fun body i =
            runM (f(g ($ i),E($ a))) a
        val p = a := e >>
                For(n, body)
    in (E($ a),p)
    end
  | foldl f (V _) _     = die "foldl: expecting expression as accumulator"
  | foldl f _     (E _) = die "foldl: expecting vector to iterate over"

fun foldr f (E e) (V(n,g)) =
    let open P
        val a = Name.new ()
        val n0 = Name.new ()                   
        fun body i =
            runM (f(g ($ n0 - $ i),E($ a))) a
        val p = n0 := n - I 1 >>
                a := e >>
                For(n, body)
    in (E($ a),p)
    end
  | foldr f (V _) _ = die "foldr: expecting expression as accumulator"
  | foldr f _ (E _) = die "foldr: expecting vector to iterate over"
                      
fun concat (V(n1,f1)) (V(n2,f2)) =
    let open P
        fun unE (E e) = e
          | unE (V _) = die "concat: expecting expression"
    in V(n1+n2, fn i => E(If(i < n1, unE(f1 i), unE(f2 (i-n1)))))
    end
  | concat _ _ = die "concat: expecting vectors"

fun fromList nil = empty()
  | fromList [t] = single t
  | fromList (t::ts) = concat (single t) (fromList ts)
                       
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
