(* Pull vectors *)

signature TERM = sig
  type t0
  type 'a M = 'a * (Program.ss -> Program.ss)
  val E : Program.e -> t0
  val V : IL.Type * Program.e * (Program.e -> t0 M) -> t0
  val unE : t0 -> Program.e option
  val unV : t0 -> (IL.Type * Program.e * (Program.e -> t0 M)) option
end

functor ILvec(Term : TERM) 
        : ILVEC where type 'a t = Term.t0 =
struct
open Term
open Type

type Value = IL.Value
structure P = Program

fun die s = raise Fail ("ILvec2m." ^ s)

type 'a t = t0
type 'a v = t0
infix >>=
fun (v,ssT) >>= f = let val (v',ssT') = f v in (v', ssT o ssT') end
fun ret v = (v, fn ss => ss)
fun runM0 (e,ssT) k =
    case unE e of
      SOME e => ssT [k e]
    | NONE => die "runM: expecting expression"

type 'a NUM  = 'a Num t
type INT     = Int NUM
type DOUBLE  = Double NUM
type BOOL    = Bool t

val I : int -> INT = E o P.I
val D : real -> DOUBLE = E o P.D
val B : bool -> BOOL = E o P.B

fun binop opr (t1,t2) =
    case (unE t1,unE t2) of
      (SOME t1, SOME t2) => E(opr(t1,t2))
    | _ => die "binop: expecting expressions"

fun unop opr t =
    case unE t of
      SOME t => E(opr t)
    | _ => die "unop: expecting expression"

fun curry f x y = f(x,y)
fun uncurry f (x,y) = f x y

local open P
in
  fun map ty f t =
      case unV t of
        SOME (_,n,g) => V(ty, n, fn i => g i >>= f)
      | NONE => die "map: expecting vector"

  fun stride t v =
      case (unE t, unV v) of
        (SOME n, SOME (ty,m,g)) => 
        let val n = max (I 1) n
        in V(ty, m / n, fn i => g(n * i))
        end
      | (NONE, _) => die "stride: expecting expression"
      | _ => die "stride: expecting vector"

  fun map2 ty f t1 t2 =
      case (unV t1, unV t2) of
        (SOME(_,n1,f1), SOME(_,n2,f2)) => 
        V(ty, 
          min n1 n2, fn i => 
                        f1 i >>= (fn v1 =>
                        f2 i >>= (fn v2 =>
                        f(v1,v2))))
      | _ => die "map2: expecting vectors"

  fun rev t =
      case unV t of
        SOME(ty,n,g) => V(ty,n, fn i => g(n - i - I 1))
      | NONE => die "rev: expecting vector"

  fun tabulate ty t f =
      case unE t of
        SOME n => V(ty,n,f o E)
      | NONE => die "tabulate: expecting expression"

  fun proto t =
        if t = Int then I 0
        else if t = Double then D 0.0
        else if t = Bool then B true
        else die ("proto: unsupported type " ^ prType t)

  fun dummy_exp t =
      if t = Int then I 666
      else if t = Double then D 66.6
      else if t = Bool then B false
      else die ("empty: unknown type " ^ prType t)

  fun empty ty = V(ty, I 0, fn _ => ret(E(dummy_exp ty)))

  fun emptyOf t =
      case unV t of
        SOME(ty,_,f) => V(ty, I 0, f)
      | NONE => (*V(I 0, fn _ => ret t)*) die "emptyOf: expecting vector"

  fun single ty t = V(ty, I 1, fn _ => ret t)

  fun tk t v =
      case (unE t, unV v) of
        (SOME n, SOME (ty,m,g)) => V(ty,min n m, g)
      | (NONE, _) => die "tk: expecting expression"
      | _ => die "tk: expecting vector"
             
  fun dr t v =
      case (unE t, unV v) of
        (SOME n, SOME(ty,m,g)) => V(ty,max (m-n) (I 0), fn i => g(i+n))
      | (NONE, _) => die "dr: expecting expression"
      | _ => die "dr: expecting vector"

  fun length t =
      case unV t of
        SOME (_,n,_) => E n
      | NONE => die "length: expecting vector"
end

val op +  : 'a NUM * 'a NUM -> 'a NUM = binop P.+
val op -  : 'a NUM * 'a NUM -> 'a NUM = binop P.-
val op *  : 'a NUM * 'a NUM -> 'a NUM = binop P.*
val op /  : 'a NUM * 'a NUM -> 'a NUM = binop P./
val op %  : INT * INT -> INT = binop P.%
val op <  : 'a NUM * 'a NUM -> BOOL = binop P.<
val op <= : 'a NUM * 'a NUM -> BOOL = binop P.<=
val op == : 'a NUM * 'a NUM -> BOOL = binop P.==
val max   : 'a NUM -> 'a NUM -> 'a NUM = curry(binop(uncurry P.max))
val min   : 'a NUM -> 'a NUM -> 'a NUM = curry(binop(uncurry P.min))
val ~     : 'a NUM -> 'a NUM = unop P.~
val i2d   : INT -> DOUBLE = unop P.i2d
val d2i   : DOUBLE -> INT = unop P.d2i

(* Values and Evaluation *)
type 'a V    = IL.Value
val Iv       = IL.IntV
val unIv     = fn IL.IntV i => i | _ => die "unIv"
val Dv       = IL.DoubleV
val unDv     = fn IL.DoubleV d => d | _ => die "unDv"
val Bv       = IL.BoolV
val unBv     = fn IL.BoolV b => b | _ => die "unBv"
fun Vv vs    = IL.ArrV(Vector.fromList(List.map (fn v => ref(SOME v)) vs))
fun vlist v = Vector.foldl (op ::) nil v
val unVv     = fn IL.ArrV v => List.map (fn ref (SOME a) => a
                                          | _ => die "unVv.1") (vlist v)
                | _ => die "unVv"
val Uv       = Iv 0
val ppV      = ILUtil.ppValue 

(* Some utility functions *)
fun unE' s t =
    case unE t of
      SOME e => e
    | NONE => die s

fun opt_ss0 ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls0 ss
    end

fun opt_ss e ss =
    let val ss = P.se_ss nil ss
        val ss = P.se_ss nil ss
    in P.rm_decls (unE' "opt_ss" e) ss
    end

infix >> ::=
val op := = P.:=

type ('a,'b) prog = 'a Type.T * 'b Type.T * (Name.t * (P.e -> P.s) -> P.ss)

fun pp_prog ((ta,tb,p): ('a,'b) prog) : string =
    let val name_arg = Name.new ta
        val ss = p (name_arg, P.Ret)
        val ss = opt_ss0 ss
    in ILUtil.ppFunction "kernel" (ta,tb) name_arg ss
    end

fun eval ((ta,tb,p): ('a,'b) prog) (v: 'a V) : 'b V =
    let val name_arg = Name.new ta
        val ss = p (name_arg, P.Ret)
        val ss = opt_ss0 ss

        val () = print (ILUtil.ppFunction "kernel" (ta,tb) name_arg ss)
(*
        val () = print ("Program(" ^ Name.pr name_arg ^ ") {" ^ 
                        ILUtil.ppProgram 1 program ^ "\n}\n")
*)
        val name_res = Name.new tb
        val env0 = ILUtil.add ILUtil.emptyEnv (name_arg,v)
        val env = ILUtil.evalSS env0 ss name_res      
    in case ILUtil.lookup env name_res of
         SOME v => v
       | NONE => die ("Error finding '" ^ Name.pr name_res ^ 
                      "' in result environment for evaluation of\n" ^
                      ILUtil.ppSS 0 ss)
    end

fun runF (ta,tb) (f: 'a t -> 'b t M) =
    (ta,
     tb,
     fn (n0,k) =>
        let val (e,ssT) = f (E(IL.Var n0))
        in case unE e of
             SOME e => ssT [k e]
           | NONE => die "runM: expecting expression"
        end)

fun runM ta (e,ssT) =
  (Type.Int,
   ta,
   fn (_,k) => runM0 (e,ssT) k)

fun typeComp s c =
    let val (e,_) = c
    in ILUtil.typeExp(unE' ("typeComp." ^ s ^ ": only plain computations are supported") e)
    end

val $ = E o P.$

fun get_exp (m1 as (e,f)) =
    let val ss = f nil
        val ss = opt_ss e ss
    in case ss of
         nil => SOME e
       | _ =>
         let (*val () = print("no_get:" ^ ILUtil.ppSS 0 ss ^ "\n")*)
         in NONE
         end
    end

fun If0 (x,m1,m2) =
    let val t = typeComp "If" m1
        val n = Name.new t
    in case (get_exp m1, get_exp m2) of
         (SOME e1,SOME e2) => 
         ($ n, fn ss => P.Decl(n,SOME(P.If(x,unE' "If0.true" e1,
                                           unE' "If0.false" e2)))::ss)
       | _ =>
         let val k = fn v => n := v
             val s1 = runM0 m1 k
             val s2 = runM0 m2 k
             val s1 = opt_ss0 s1
             val s2 = opt_ss0 s2
             fun default() =
                 ($ n, fn ss => P.Decl(n,NONE)::P.Ifs(x,s1,s2) ss)
         in case (s1, s2) of
              ([IL.Assign(n1,e1)],[IL.Assign(n2,e2)]) =>
              if n = n1 andalso n = n2 then
                ($ n, fn ss => P.Decl(n,SOME(P.If(x,e1,e2)))::ss)
              else default()
            | _ => default()
         end
    end

fun If (x0,a1,a2) =
    case (unE x0, unV a1, unV a2) of
      (SOME x, SOME (ty,n1,f1), SOME (_,n2,f2)) =>
      V(ty,P.If(x,n1,n2), 
        fn i =>
           let val m1 = f1 i
               val m2 = f2 i
           in If0(x,m1,m2)
           end)
    | _ =>
      case (unE x0, unE a1, unE a2) of
        (SOME x, SOME a1, SOME a2) => E(P.If(x,a1,a2))
      | _ => die "If: expecting branches to be of same kind and cond to be an expression"

fun LetI v f = f v

fun memoize t =
    case unV t of
      SOME (ty,n,f) =>
      let open P
          val tyv = Type.Vec ty
          val name = Name.new tyv
          fun ssT ss = Decl(name, SOME(Alloc(tyv,n))) ::
                       (For(n, fn i => runM0(f i)(fn v => (name,i) ::= v)) ss)
      in (V(ty,n, fn i => ret(E(Subs(name,i)))), ssT)
      end
    | _ => die "memoize: expecting vector"
           
fun For'(n,e,body) =
    let open P
    in case unI n of
         SOME 0 => (E e, fn ss => ss)
       | SOME 1 => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = Decl(a, SOME e)
             val ss = body e f (I 0)
         in case ss of
              [s] => 
              (case unDecl s of
                 SOME (a',SOME e) => if a' = a then (E e, fn ss => ss)
                                     else die "For': weird"
               | SOME (a',NONE) => die "For': weird2"
               | NONE => (E($ a), fn ss => s :: ss))
            | ss0 => (E($ a), fn ss => ss0 @ ss)
         end
       | _ => 
         let val ty = ILUtil.typeExp e
             val a = Name.new ty
             fun f e = a := e
         in (E($ a), fn ss => 
                        Decl(a, SOME e) ::
                        For(n, body ($ a) f) ss)
         end
    end

fun foldl f e v =
    let open P
    in case (unE e, unV v) of
         (SOME e, SOME (_,n,g)) =>
         let fun body e h i =
                 runM0 (g i >>= (fn v => f(v,E e))) h
         in For'(n,e,body)
         end
       | (NONE, SOME (_,n,g)) =>
         (case unI n of
            SOME n =>
            let fun loop i a = if i >= n then ret a
                               else g (I i) >>= (fn v => 
                                    f (v,a) >>= (fn x => 
                                    loop(Int.+(i,1)) x))
            in loop 0 e
            end
          | _ => die "foldl: expecting expression as accumulator or index vector to be constant-sized")
       | (_, NONE) => die "foldl: expecting vector to iterate over"
    end

fun foldr f e v =
      case (unE e, unV v) of
        (SOME e, SOME (_,n,g)) =>
        let open P
            fun body e h i =
                runM0 (g (n - I 1 - i) >>= (fn v => f(v,E e))) h
        in For'(n,e,body)
        end
      | (NONE, _) => die "foldr: expecting expression as accumulator"
      | (_, NONE) => die "foldr: expecting vector to iterate over"
                     
fun concat v1 v2 =
    case (unV v1, unV v2) of
      (SOME(ty,n1,f1), SOME(_,n2,f2)) => 
      (case P.unI n1 of
         SOME 0 => v2
       | _ =>
         case P.unI n2 of
           SOME 0 => v1
         | _ => V(ty,P.+(n1,n2), 
                  fn i => 
                     let val m1 = f1 i
                         val m2 = f2 (P.-(i,n1))
                     in If0(P.<(i,n1),m1,m2)
                     end))
    | _ => die "concat: expecting vectors"
             
  fun fromList ty nil = empty Int
    | fromList ty [t] = single ty t
    | fromList ty (t::ts) = concat (single ty t) (fromList ty ts)

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

  infix ==
  fun eq f v1 v2 =
      let val v = map2 Bool (ret o f) v1 v2
          val base = length v1 == length v2
      in foldr (fn (b,a) => ret(If(a,b,a))) base v
      end

  fun sub_unsafe v i =
    case (unV v, unE i) of
      (SOME (_,n,g), SOME i) => g i
    | _ => die "sub_unsafe: expecting vector and integer"

  fun tyOfV v =
      case unV v of
          SOME (ty,_,_) => ty
        | NONE => die "tyOfV: expecting vector"

  fun merge v n t =
      concat (tk (n - I 1) v)
      (concat (single (tyOfV v) t) (dr (n + I 1) v))

  infix %
              
  fun trans v d =
      case (unV v, unV d) of
        (SOME (_,n,f), SOME(ty,m,g)) =>
        (case P.unI n of
           SOME 0 => d   (* known number of dimensions *)
         | SOME 1 => d
         | SOME 2 =>
           let fun g' a =
                   f (P.I 0) >>= (fn N =>
                   f (P.I 1) >>= (fn M =>
                   let val a = E a
                       val t = M * N - I 1
                       val r = If(a == t, a, (M*a) % t)
                       val r = case unE r of
                                  SOME x => x
                                | NONE => die "trans:impossible"
                   in g r
                   end))
           in V(ty,m,g')
           end
         | SOME n => die ("trans: not implemented - " ^ Int.toString n)
         | NONE => die "trans: unknown number of dimensions not supported")
      | _ => die "trans: expecting vectors"

  fun extend n v =
      case (unE n, unV v) of
        (SOME n', SOME(ty,m,f)) =>
        If(E m == I 0, V(ty,n', fn _ => ret (E(proto ty))), V(ty,n',f o (fn i => P.%(i, m))))
      | _ => die "extend: expecting term and array"

  fun outmain outln =
    ( outln "int main() {"
    ; outln "  printf(\"%f\\n\", kernel(0));"
    ; outln "  return 0;"
    ; outln "}")

  fun outprog ofile p =
    let val body = pp_prog p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln "#include <stdio.h>"
     ; outln "#include <stdlib.h>"
     ; outln "#include <math.h>"
     ; outln "#include \"apl.h\""
     ; outln body
     ; outmain outln
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end


end

structure Term = struct
  type 'a M = 'a * (Program.ss -> Program.ss)
  datatype t0 = E of Program.e
              | V of IL.Type * Program.e * (Program.e -> t0 M)
  fun unE (E e) = SOME e
    | unE _ = NONE
  fun unV (V v) = SOME v
    | unV _ = NONE
end

structure ILvec :> ILVEC = ILvec(Term)
