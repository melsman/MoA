(* Pull vectors *)

signature TERM = sig
  type t0
  val E : Program.e -> t0
  val V : Program.e * (Program.e -> t0) -> t0
  val unE : t0 -> Program.e option
  val unV : t0 -> (Program.e * (Program.e -> t0)) option
end

functor ILvec(Term : TERM) 
        : ILVEC where type 'a t = Term.t0
                  and type 'a T = string =
struct
open Term

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
 
type 'a t = t0
type 'a v = t0

type INT     = Int t
type BOOL    = Bool t

val I : int -> INT = E o P.I
val B : bool -> BOOL = E o P.B

fun binop opr (t1,t2) =
    case (unE t1,unE t2) of
      (SOME t1, SOME t2) => E(opr(t1,t2))
    | _ => die "binop: expecting expressions"

fun curry f x y = f(x,y)
fun uncurry f (x,y) = f x y

local open P
in
  fun map f t =
      case unV t of
        SOME (n,g) => V(n, f o g)
      | NONE => die "map: expecting vector"

  fun map2 f t1 t2 =
      case (unV t1, unV t2) of
        (SOME(n1,f1), SOME(n2,f2)) => V(min n1 n2, fn i => f(f1 i,f2 i))
      | _ => die "map2: expecting vectors"

  fun rev t =
      case unV t of
        SOME(n,g) => V(n, fn i => g(n - i - I 1))
      | NONE => die "rev: expecting vector"

  fun tabulate t f =
      case unE t of
        SOME n => V(n,f o E)
      | NONE => die "tabulate: expecting expression"

  fun dummy_exp "Int" = I 666
    | dummy_exp "Bool" = B false
    | dummy_exp ty = die ("empty: unknown type " ^ ty)

  fun empty ty = V(I 0, fn _ => E(dummy_exp ty))

  fun emptyOf t =
      case unV t of
        SOME(_,f) => V(I 0, f)
      | NONE => V(I 0, fn _ => t) (* die "emptyOf: expecting vector" *)

  fun single t = V(I 1, fn _ => t)

  fun tk t v =
      case (unE t, unV v) of
        (SOME n, SOME (m,g)) => V(min n m, g)
      | (NONE, _) => die "tk: expecting expression"
      | _ => die "tk: expecting vector"
             
  fun dr t v =
      case (unE t, unV v) of
        (SOME n, SOME(m,g)) => V(max (m-n) (I 0), fn i => g(i+n))
      | (NONE, _) => die "dr: expecting expression"
      | _ => die "dr: expecting vector"

  fun length t =
      case unV t of
        SOME (n,_) => E n
      | NONE => die "length: expecting vector"
end

val op +  : INT * INT -> INT = binop P.+
val op -  : INT * INT -> INT = binop P.-
val op *  : INT * INT -> INT = binop P.*
val op <  : INT * INT -> BOOL = binop P.<
val op <= : INT * INT -> BOOL = binop P.<=
val op == : INT * INT -> BOOL = binop P.==
val max   : INT -> INT -> INT = curry(binop(uncurry P.max))
val min   : INT -> INT -> INT = curry(binop(uncurry P.min))

(* Values and Evaluation *)
type 'a V    = IL.Value
val Iv       = IL.IntV
val unIv     = fn IL.IntV i => i
                | _ => die "unIv"
val Bv       = IL.BoolV
val unBv     = fn IL.BoolV b => b
                | _ => die "unBv"
fun Vv vs    = IL.ArrV(Vector.fromList(List.map (fn v => ref(SOME v)) vs))
fun vlist v = Vector.foldl (op ::) nil v
val unVv     = fn IL.ArrV v => List.map (fn ref (SOME a) => a
                                          | _ => die "unVv.1") (vlist v)
                | _ => die "unVv"
val Uv       = Iv 0
val ppV      = ILUtil.ppValue 

type 'a M = 'a * P.p
infix >>= >> ::=
val op >> = P.>>
val op := = P.:=

type ('a,'b) prog = Name.t * (P.e -> P.p) -> P.p

fun eval (p: ('a,'b) prog) (v: 'a V) : 'b V =
    let val name_arg = Name.new ()
        val program = p (name_arg, P.Ret)
        val () = print (ILUtil.ppFunction "kernel" name_arg program)
(*
        val () = print ("Program(" ^ Name.pr name_arg ^ ") {" ^ 
                        ILUtil.ppProgram 1 program ^ "\n}\n")
*)
        val env0 = ILUtil.add ILUtil.emptyEnv (name_arg,v)
        val env = ILUtil.evalProgram env0 program        
    in case ILUtil.lookup env Name.result of
         SOME v => v
       | NONE => die ("Error finding '" ^ Name.pr Name.result ^ 
                      "' in result environment for evaluation of\n" ^
                      ILUtil.ppProgram 0 program)
    end

fun (v,p) >>= f = let val (v',p') = f v in (v',p >> p') end
fun ret v = (v, P.emp)

fun runF f (n0,k) =
    let val (e,p) = f (E(IL.Var n0))
    in case unE e of
         SOME e => p >> k e
       | NONE => die "runM: expecting expression"
    end

fun runM0 (e,p) k =
    case unE e of
      SOME e => p >> k e
    | NONE => die "runM: expecting expression"

fun runM (e,p) (_,k) = runM0 (e,p) k

fun memoize t =
    case unV t of
      SOME (n,f) =>
      let open P
          val name = Name.new ()
          val f = fn i => case unE (f i) of 
                            SOME e => e 
                          | _ => die "memoize"
          val p = name := Alloc n >>
                  For(n, fn i => (name,i) ::= f i)
      in (V(n, fn i => E(Subs(name,i))), p)
      end
    | _ => die "memoize: expecting vector"

fun foldl f e v =
    let open P
    in case (unE e, unV v) of
         (SOME e, SOME (n,g)) =>
         let val a = Name.new ()
             fun body i =
                 runM0 (f(g i,E($ a))) (fn e => a := e)
             val p = a := e >>
                     For(n, body)
         in (E($ a),p)
         end
       | (NONE, SOME (n,g)) =>
         (case unI n of
            SOME n =>
            let fun loop i a = if i >= n then ret a
                               else f (g (I i), a) >>= (fn x => loop(Int.+(i,1)) x)
            in loop 0 e
            end
          | _ => die "foldl: expecting expression as accumulator or index vector to be constant-sized")
       | (_, NONE) => die "foldl: expecting vector to iterate over"
    end
     
fun foldr f e v =
    case (unE e, unV v) of
      (SOME e, SOME (n,g)) =>
      let open P
          val a = Name.new ()
          val n0 = Name.new ()                   
          fun body i =
              runM0 (f(g ($ n0 - i),E($ a))) (fn e => a := e)
          val p = n0 := n - I 1 >>
                  a := e >>
                  For(n, body)
      in (E($ a),p)
      end
  | (NONE, _) => die "foldr: expecting expression as accumulator"
  | (_, NONE) => die "foldr: expecting vector to iterate over"

fun If(x0,a1,a2) =
    case (unE x0, unV a1, unV a2) of
      (SOME x, SOME (n1,f1), SOME (n2,f2)) =>
      V(P.If(x,n1,n2), 
        fn i =>
           let val (x1, x2) = (f1 i, f2 i)
           in case (unE x1, unE x2) of
                (SOME v1, SOME v2) => E(P.If(x,v1,v2))
              | _ => If(x0,x1,x2)
           end)
    | _ =>
    case (unE x0, unE a1, unE a2) of
      (SOME x, SOME a1, SOME a2) => E(P.If(x,a1,a2))
    | _ => die "If: expecting branches to be of same kind and cond to be an expression"
                      
fun concat v1 v2 =
    case (unV v1, unV v2) of
      (SOME(n1,f1), SOME(n2,f2)) => 
      (case P.unI n1 of
         SOME 0 => v2
       | _ =>
       case P.unI n2 of
         SOME 0 => v1
       | _ => V(P.+(n1,n2), fn i => If(E(P.<(i,n1)), f1 i, f2 (P.-(i,n1)))))
    | _ => die "concat: expecting vectors"

fun fromList nil = empty Int
  | fromList [t] = single t
  | fromList (t::ts) = concat (single t) (fromList ts)

fun assert_vector s v =
    case unV v of
      SOME _ => ()
    | NONE => die ("assert_vector: " ^ s)
                       
fun flatten ty v =
    (assert_vector "flatten" v;
     foldl (fn (v,a) =>
               (assert_vector "flatten_a" a;
                assert_vector "flatten_v" v;
                ret(concat a v))) (empty ty) v)

fun flattenOf v0 v =
    (assert_vector "flatten" v;
     foldl (fn (v,a) =>
               (assert_vector "flatten_a" a;
                assert_vector "flatten_v" v;
                ret(concat a v))) (emptyOf v0) v)


(*
  fun flatten v =
      let val m = foldl (fn (a,l) => length a + l) (I 0) v >>= (fn len =>
	          (foldr (fn ((n,f),g) =>
		          fn i => If(i < n, f i, g (i-n))) emp v >>= (fn f =>
      in (len,f)
      end
*)

infix ==
fun eq f v1 v2 =
    let val v = map2 f v1 v2
        val base = length v1 == length v2
    in foldr (fn (b,a) => ret(If(a,b,a))) base v
    end

fun iter x y f = if x > y then nil
                 else f x :: iter (Int.+(x,1)) y f

fun shapify t =
    case unV t of
      SOME (n,f) =>
      (case P.unI n of
         SOME n => (* length is immediate *)
         let val xs = iter 0 (Int.-(n,1)) (fn i => f (P.I i))
             val contains0 = List.exists (fn x => case unE x of
                                                    SOME e => P.unI e = SOME 0
                                                  | NONE => false) xs
             exception NonImmed
         in if contains0 then single(I 0)
            else t (*let val immeds =
                         List.map (fn x => case unE x of
                                             SOME e =>
                                             (case P.unI e of
                                                SOME n => n
                                              | NONE => raise NonImmed)
                                           | NONE => raise NonImmed) xs
                     val non1s = 
                         List.filter (fn x => x <> 1) immeds
                 in fromList(List.map I non1s)
                 end
                 handle NonImmed => t *)
         end
       | NONE => t)
    | NONE => t

(*val shapify = fn x => x*)

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
structure Term = struct
  datatype t0 = E of Program.e
              | V of Program.e * (Program.e -> t0)
  fun unE (E e) = SOME e
    | unE _ = NONE
  fun unV (V v) = SOME v
    | unV _ = NONE
end

structure ILvec :> ILVEC = ILvec(Term)
