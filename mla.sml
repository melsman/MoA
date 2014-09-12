structure Mla : ILAPL = struct

structure Ty : EXP_TYPE = struct
type var = string
type opr = string
open URef
type bv = string
type tv = string
type rv = string

datatype r = R of int
           | Rv of rv * (int->string option)
withtype rnk = r uref
datatype b = IntT
           | DoubleT
           | BoolT
           | Bv of bv 
withtype bty = b uref
datatype t = ShT of rnk
           | SiT of rnk
           | ViT of rnk
           | ArrT of bty * rnk
           | FunT of typ * typ 
           | TyvT of tv
withtype typ = t uref

local
  fun newcount s =
      let val c = ref 0
      in fn () => s ^ Int.toString(!c before c:= !c + 1)
      end
in fun RnkVarCon f : rnk = uref(Rv(newcount "'r" (),f))
   fun RnkVar ()   : rnk = RnkVarCon (fn _ => NONE)
   fun TyVarB ()   : bty = uref(Bv(newcount "'b" ()))
   fun TyVar ()    : typ = uref(TyvT(newcount "'a" ()))
end

fun rnk n = uref (R n)
val rnk0 = rnk 0
val rnk1 = rnk 1
fun unRnk r = case !!r of R i => SOME i | _ => NONE

local fun Scl b = uref (ArrT(b,rnk0))
in val IntB    = uref IntT
   val DoubleB = uref DoubleT
   val BoolB   = uref BoolT
   val Int     = Scl IntB
   val Double  = Scl DoubleB
   val Bool    = Scl BoolB
end
fun Sh r = uref(ShT r)
fun Si r = uref(SiT r)
fun Vi r = uref(ViT r)
fun Arr bt r = uref(ArrT(bt,r))
fun Fun (t1,t2) = uref(FunT(t1,t2))

fun prR r = case r of R i => Int.toString i | Rv (rv,_) => rv
and prRnk r = prR (!!r)
and prB b =
    case b of
        IntT => "int"
      | DoubleT => "double"
      | BoolT => "bool"
      | Bv bv => bv
and prBty bty = prB(!!bty)
and prT t =
    case t of
        ShT r => "Sh(" ^ prRnk r ^ ")"
      | SiT r => "Si(" ^ prRnk r ^ ")"
      | ViT r => "Vi(" ^ prRnk r ^ ")"
      | ArrT (bt,r) => "[" ^ prBty bt ^ "]" ^ prRnk r
      | FunT (t1,t2) => "(" ^ prTy t1 ^ ")->" ^ prTy t2
      | TyvT tv => tv
and prTy t = prT(!!t)
val prType = prTy

fun unArr t = case !!t of ArrT p => SOME p | _ => NONE
fun unSh t = case !!t of ShT r => SOME r | _ => NONE
fun unSi t = case !!t of SiT r => SOME r | _ => NONE
fun unVi t = case !!t of ViT r => SOME r | _ => NONE
fun unFun t = case !!t of FunT p => SOME p | _ => NONE

fun comb f1 f2 t = case f1 t of NONE => f2 t | x => x
fun check f t = case f t of SOME s => raise Fail s | NONE => ()
fun isInt bt = case !!bt of IntT => true | _ => false
fun isDouble bt = case !!bt of DoubleT => true | _ => false
fun isBool bt = case !!bt of BoolT => true | _ => false

fun combB (b1,b2) =
    case (b1,b2) of
        (Bv _, _) => b2
      | (_, Bv _) => b1
      | (IntT, IntT) => b1
      | (DoubleT, DoubleT) => b1
      | (BoolT, BoolT) => b1
      | _ => raise Fail ("cannot unify " ^ prB b1 ^ " and " ^ prB b2)
and unifB b1 b2 = URef.unify combB (b1,b2)
and combT (t1,t2) =
    case (t1,t2) of
        (TyvT _, _) => t2
      | (_, TyvT _) => t1
      | (t as ArrT (b1,r1), ArrT (b2,r2)) => (unifB b1 b2; unifR r1 r2; t) 
      | (t as FunT (t1,t2), FunT (t1',t2')) => (unif t1 t1'; unif t2 t2'; t)
      | (ShT r1, ShT r2) => (unifR r1 r2; t1)
      | (SiT r1, SiT r2) => (unifR r1 r2; t1)
      | (ViT r1, ViT r2) => (unifR r1 r2; t1)
      | _ => raise Fail ("cannot unify " ^ prT t1 ^ " and " ^ prT t2)
and unif t1 t2 = URef.unify combT (t1,t2)
and combR (r1,r2) =
    case (r1,r2) of
        (R i1, R i2) => if i1 = i2 then r1
                        else raise Fail ("cannot unify rank " ^ prR r1 ^ " and rank " ^ prR r2)
      | (Rv(rv1,f1), Rv(_,f2)) => Rv(rv1,comb f1 f2)
      | (Rv(_,f), R i) => (check f i; r2)
      | (R i, Rv(_,f)) => (check f i; r1)
and unifR r1 r2 = URef.unify combR (r1,r2)

fun relateR _ = raise Fail "relateR not implemented"
fun relateR2 _ = raise Fail "relateR2 not implemented"

fun Vec t =
    let val bt = TyVarB()
        val r = rnk0
    in unif t (Arr bt r);
       Arr bt rnk1
    end

fun wrap f x y = (f x y; NONE) handle Fail s => SOME s
val unify = wrap unif
val unifyR = wrap unifR
val unifyB = wrap unifB

fun subtype t1 t2 =
    case unSi t1 of
        SOME r1 => (case unSi t2 of
                        SOME r2 => unifyR r1 r2
                      | NONE => unify t2 Int)
      | NONE => 
        case unVi t1 of
            SOME r1 => (case unVi t2 of
                            SOME r2 => unifyR r1 r2
                          | NONE => case unSh t2 of
                                        SOME r => unifyR rnk1 r
                                      | NONE => unify t2 (Vec Int))
          | NONE =>
            case unSh t1 of
                SOME r1 => (case unSh t2 of
                                SOME r2 => unifyR r1 r2
                              | NONE => unify t2 (Vec Int))
              | NONE => unify t1 t2
end            

structure Exp = Exp(Ty)
open Ty
open Exp
type Int = unit
type Double = unit
type 'a Num = unit
type Bool = unit
type 'a Vec = unit
type 'a T = typ
fun vecElem t = raise Fail "vecElem"
(*
    case unArr t of
        SOME (t,r) => (unifr r rnk1; t)
      | NONE => raise Fail "vecElem: impossible" 
*)
fun vecLength e =
    case e of
        Vc(es,_) => SOME(List.length es)
      | _ => NONE

(* Expressions *)
fun binOp opr (x,y) = Op_e(opr,[x,y])
fun binOp' opr x y = Op_e(opr,[x,y])
val addi = binOp "addi"
val subi = binOp "subi"
val muli = binOp "muli"
val divi = binOp "divi"
val lti  = binOp "lti"
val leqi = binOp "leqi"
val eqi  = binOp "eqi"
val maxi = binOp' "maxi"
val mini = binOp' "mini"
fun negi x = Op_e("negi",[x])
val addd = binOp "addd"
val subd = binOp "subd"
val muld = binOp "muld"
val divd = binOp "divd"
val ltd  = binOp "ltd"
val leqd = binOp "leqd"
val eqd  = binOp "eqd"
val maxd = binOp' "maxd"
val mind = binOp' "mind"
fun negd x = Op_e("negd",[x])

val i2d = fn x => Op_e ("i2d", [x])
val op % = binOp "mod"

fun If (b,e1,e2) = Iff_e(b,e1,e2)

val fromList = fn _ => Vc_e
val op + = fn (x,y) => Op_e ("add", [x,y])
val op - = fn (x,y) => Op_e ("sub", [x,y])
val op * = fn (x,y) => Op_e ("mul", [x,y])
val op / = fn (x,y) => Op_e ("div", [x,y])
val op < = fn (x,y) => Op_e ("lt", [x,y])
val op <= = fn (x,y) => Op_e ("leq", [x,y])
val op == = fn (x,y) => Op_e ("eq", [x,y])
val max = fn x => fn y => Op_e ("max", [x,y])
val min = fn x => fn y => Op_e ("min", [x,y])
val ~ = fn x => Op_e ("neg", [x])

type 'a t = exp
type 'a v = exp
type 'a NUM = exp
type INT = exp
type DOUBLE = exp
type BOOL = exp
                
type 'a M = ('a -> exp) -> exp
                               
fun ret (v:'a) : 'a M = fn k => k v

infix >>=
fun (f : 'a M) >>= (g : 'a -> 'b M) : 'b M =
   fn k => f (fn x => g x k)

fun fromListM t x = ret(fromList t x)

(* Compiled Programs *)
type ('a,'b) prog = exp
fun runF _ f = f (Var("arg",TyVar())) (fn x => x)
 
(* Values and Evaluation *)
type 'a V = Exp.value
fun Iv _ = raise Fail "mla.Iv"
fun unIv _ = raise Fail "mla.unIv"
val Dv = Exp.Dvalue
val unDv = Exp.unDvalue
fun Bv _ = raise Fail "mla.Bv"
fun unBv _ = raise Fail "mla.unBv"
fun Vv _ = raise Fail "mla.Vv"
fun unVv _ = raise Fail "mla.unVv"
val Uv = Exp.Uvalue

local val c = ref 0
in fun newVar() = "v" ^ Int.toString (!c) before c := Int.+(!c,1)
end

type 'a MVec = unit
type 'a m = exp
fun zilde _ = Op_e("zilde",nil)
fun scl _ t = t
fun vec t = t
fun iota t = Op_e("iota",[t])
fun iota' t = Op_e("iota",[t])
fun first t = Op_e("first",[t])
fun siz t = Op_e("siz",[t])
fun dim t = Op_e("dim",[t])   (* or I(#2 t) *)
fun rav t = Op_e("rav",[t])
fun rav0 t = t
fun each t _ f e =
    let val v = newVar()
        val e0 = f (Var(v,t)) (fn x => x)
    in Op_e("each",[Fn_e(v,t,e0),e])
    end
fun mkFn2 t1 t2 f =
    let val (v1, v2) = (newVar(), newVar())
        val e0 = f (Var(v1,t1), Var(v2,t2))
    in Fn_e(v1,t1,Fn_e(v2,t2,e0))
    end
fun mkFn2m t1 t2 f = mkFn2 t1 t2 (fn a => f a (fn x=>x))
fun bin t1 t2 s f e1 e2 =
    ret(Op_e(s,[mkFn2 t1 t2 f,e1,e2]))
fun binm t1 t2 s f e1 e2 = 
    bin t1 t2 s (fn a => f a (fn x => x)) e1 e2
fun red t1 t2 f n e = binm t1 t2 "red" f n e
fun meq t f e1 e2 = bin t t "meq" f e1 e2
fun mif (b,e1,e2) = If(b,e1,e2)
fun sum t1 t2 _ f e1 e2 = binm t1 t2 "sum" f e1 e2
fun scan t1 t2 f e1 e2 = bin t1 t2 "scan" f e1 e2
fun getRank s e =
    let fun fail s = raise Fail ("rank error: " ^ s ^ 
                                 " not supported for arguments of unknown rank")
        val t = typeOf e
    in case unSi t of
           SOME _ => 0
         | NONE =>
       case unVi t of
           SOME _ => 1
         | NONE =>
       case unSh t of
           SOME _ => 1 
         | NONE => 
       case unArr t of
           SOME (_, r) => (case unRnk r of SOME i => i
                                         | NONE => fail s)
         | NONE => fail s
    end
fun catenate e1 e2 =
    let fun cat () = ret(Op_e("cat", [e1,e2]))
        fun cons () = ret(Op_e("cons",[e1,e2]))
        fun snoc () = ret(Op_e("snoc",[e1,e2]))
        open Int
    in case (getRank "cat" e1, getRank "cat" e2) of
           (0, 0) => ret(Vc_e[e1,e2])
         | (r1, r2) => if r2=r1+1 then cons()
                       else if r1=r2+1 then snoc()
                       else if r1=r2 then cat()
                       else raise Fail ("rank error: incompatible argument ranks for catenate: " 
                                        ^ Int.toString r1 ^ " and " ^ Int.toString r2)
    end
fun abs i = let open Int
            in if i < 0 then ~ i else i
            end
fun take e1 e2 = Op_e("take", [e1,e2])
fun drop e1 e2 = Op_e("drop", [e1,e2])
fun mem e = ret(Op_e("mem",[e]))
fun rotate e1 e2 = Op_e("rotate", [e1,e2])
fun reshape e1 e2 = ret(Op_e("reshape", [e1,e2]))
fun shape e = Op_e("shape",[e])
fun prod t f g e m1 m2 s a =
    let open Int
        val r = case (getRank "prod" m1, getRank "prod" m2) of
                    (0,_) => raise Fail "rank error: prod1"
                  | (_,0) => raise Fail "rank error: prod2"
                  | (r1,r2) => r1+r2-2
        val res = Op_e("prod",[mkFn2m t t f,mkFn2m t t g,e,m1,m2])
    in if r < 0 then raise Fail "rank error: prod3"
       else if r = 0 then ret(s res)
       else ret(a res)
    end
fun reduce t f e1 e2 s a =
    let open Int
    in case getRank "reduce" e2 of
           0 => ret(s e2)
         | r => binm t t "reduce" f e1 e2 >>= 
                     (fn e => ret(if r=1 then s e else a e))
    end
fun transpose e = Op_e("transp", [e])
fun transpose2 e1 e2 = Op_e("transp2", [e1,e2])
fun reverse e = Op_e("reverse", [e])
fun catenate_first e1 e2 =
    catenate (transpose e1) (transpose e2) >>= (ret o transpose)
fun lett _ e = 
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

fun letm _ e =
    let val v = newVar()
        val t = typeOf e
    in fn f => Let_e(v,t,e,f(Var(v,t)))
    end

val letm_asgn = letm

(* Optimization *)


structure M = StringFinMap
structure Optimize = struct

type def = {shape: Exp.exp option, value: Exp.exp option}
type env = def M.map

fun getShape (E:env) (e : Exp.exp) : Exp.exp option =
    case e of
        Op("reshape",[sh,e],_) => SOME sh
      | Var(v,_) => (case M.lookup E v of
                         SOME{shape=SOME sh,...} => SOME sh
                       | _ => NONE)
      | _ => NONE

fun optimize optlevel e =
    if Int.<= (optlevel, 0) then e
    else
    let 
      fun add E k v = E
      fun opt E e =
            case e of
                Var (v,_) => e
              | I i => e
              | D r => e
              | B b => e
              | Iff (c,e1,e2,t) => Iff(opt E c,opt E e1,opt E e2,t)
              | Vc(es,t) => Vc (opts E es,t)
              | Op(opr,es,t) =>
                (case (opr, opts E es) of
                     ("addi", [I 0,e]) => e
                   | ("addi", [e,I 0]) => e
                   | ("addi", [I i1,I i2]) => I(Int.+(i1,i2))
                   | ("negi", [I i]) => I(Int.~ i)
                   | ("i2d", [I i]) => D(real i)
                   | ("reduce", [f,n,Op("zilde",[],_)]) => n
                   | ("shape", [e]) => 
                     (case getShape E e of
                          SOME e => e
                        | NONE => Op(opr,[e],t))
                   | (_,es) => Op(opr,es,t))
              | Let (v,ty,e1,e2,t) => 
                let val e1 = opt E e1
                    val sh = getShape E e1
                    val E' = M.add(v,{shape=sh,value=NONE},E)
                    val e2 = opt E' e2
                in Let(v,ty,e1,e2,t)
                end
              | Fn (v,t,e,t') => 
                let val E' = M.add(v,{shape=NONE,value=NONE},E)
                in Fn(v,t,opt E' e,t')
                end
      and opts E es = List.map (opt E) es
      val initE = M.empty
    in opt initE e
    end
end

(* Pretty printing *)

fun prOpr t opr =
    case (opr       , unSi t   , unVi t   , unSh t   ) of
         ("first"   , SOME _   , _        , _        ) => opr ^ "Sh"
      |  ("shape"   , _        , SOME _   , _        ) => opr ^ "Sh"
      |  ("take"    , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("drop"    , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("cat"     , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("cons"    , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("snoc"    , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("iota"    , _        , _        , SOME _   ) => opr ^ "Sh"
      |  ("rotate"  , _        , _        , SOME _   ) => opr ^ "Sh"
      | _ => opr

fun pp_exp e =
    let infix @@
        fun isVars nil nil = true
          | isVars (v::vs) (Var(v',_)::es) = v=v' andalso isVars vs es
          | isVars _ _ = false
        fun lookForOp vs e =
            case e of
                Fn(v,_,e',_) => lookForOp (v::vs) e'
              | Op(opr,es,t) => if isVars (rev vs) es then SOME (opr,t)
                                else NONE
              | _ => NONE
        val op + = Int.+
        val op - = Int.-
        datatype t = @@ of t * t | $ of string
        fun flatten t =
            let fun f t a =
                    case t of $ s => s::a
                            | t1 @@ t2 => f t1 (f t2 a)
            in String.concat (f t nil)
            end
        fun space 0 = ""
          | space n = " " ^ space (n-1)
        fun indent i = $("\n" ^ space i)
        fun pp i e : t =
            case e of
                Var (v,_) => $v
              | I i => $(Int.toString i)
              | D r => $(Real.fmt (StringCvt.FIX (SOME 2)) r)
              | B b => $(Bool.toString b)
              | Iff (c,e1,e2,_) => 
                let val i' = i + 2
                in $"if " @@ pp (i+3) c @@ $" then" @@
                    indent i' @@ pp i' e1 @@
                    indent i @@ $"else " @@
                    indent i' @@ pp i' e2
                end
                | Vc(es,_) => $"[" @@ pps (i+1) es @@ $"]"
                | Op (opr,nil,t) => $(prOpr t opr)
                | Op (opr,es,t) => $(prOpr t opr) @@ $"(" @@ pps (i+1+size opr) es @@ $")"
                | Let (v,ty,e1,e2,_) => $"let " @@ $v @@ $":" @@ $(prType ty) @@ $" = " @@ pp (i+2) e1 @@ $" in" @@ 
                                         indent i @@ pp i e2
                | Fn (v,t,e,_) =>
                  (case lookForOp [v] e of
                       SOME (opr,t) => $(prOpr t opr)
                     | NONE => $("fn " ^ v ^ ":" ^ prType t ^ " => ") @@ pp (i+2) e)
        and pps i nil = $""
          | pps i [e] = pp i e
          | pps i (e::es) = pp i e @@ $"," @@ pps i es
    in flatten(pp 0 e)
    end

val pp_prog = pp_exp
val ppV = Exp.pr_value

fun outprog ofile p =
    let val body = pp_prog p
        val os = TextIO.openOut ofile
        fun outln s = TextIO.output (os, s^"\n")
    in outln body
     ; TextIO.closeOut os
     ; print ("Wrote file " ^ ofile ^ "\n")
    end

fun runM {verbose,optlevel} tt m = 
    let val p = m (fn x => x)
        fun prln f =
            if verbose then (print (f()); print "\n")
            else ()
        val () = prln (fn() => "Untyped program:\n" ^ pp_prog p)
        val () = prln (fn() => "Typing the program...")
        val p = case typeExp empEnv p of
                    ERR s => raise Fail ("***Type error: " ^ s)
                  | OK t => (prln (fn() => "  Program has type: " ^ prType t);   (* perhaps unify tt with t!! *)
                             prln (fn() => "Typed program:\n" ^ pp_prog p);
                             p)
        val p = Optimize.optimize optlevel p
        val () = prln (fn() => "Optimised program:\n" ^ pp_prog p)
    in p
    end

fun eval p v =
    let val () = print ("Evaluating program\n")
        val de = Exp.addDE Exp.empDEnv "arg" v
        val v' = Exp.eval de p
    in v'
    end
    
end
