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
      | ArrT (bt,r) => "[" ^ prBty bt ^ "]" ^ prRnk r
      | FunT (t1,t2) => "(" ^ prTy t1 ^ ")->" ^ prTy t2
      | TyvT tv => tv
and prTy t = prT(!!t)
val prType = prTy

fun unArr t = case !!t of ArrT p => SOME p | _ => NONE
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
      | (ShT _, ArrT(b,r)) => (unifB IntB b; unifR r rnk1; t2)
      | (ArrT(b,r), ShT _) => (unifB IntB b; unifR r rnk1; t1)
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
        Vc(es,_) => List.length es
      | _ => ~1

(* Expressions *)
val addi = fn (x,y) => Op ("addi", [x,y], Ty.Int)
val subi = fn (x,y) => Op ("subi", [x,y], Ty.Int)
val muli = fn (x,y) => Op ("muli", [x,y], Ty.Int)
val divi = fn (x,y) => Op ("divi", [x,y], Ty.Int)
val lti = fn (x,y) => Op ("lti", [x,y], Ty.Int)
val leqi = fn (x,y) => Op ("leqi", [x,y], Ty.Int)
val eqi = fn (x,y) => Op ("eqi", [x,y], Ty.Int)
val maxi = fn x => fn y => Op ("maxi", [x,y], Ty.Int)
val mini = fn x => fn y => Op ("mini", [x,y], Ty.Int)
val negi = fn x => Op ("negi", [x], Ty.Int)

val addd = fn (x,y) => Op ("addd", [x,y], Ty.Double)
val subd = fn (x,y) => Op ("subd", [x,y], Ty.Double)
val muld = fn (x,y) => Op ("muld", [x,y], Ty.Double)
val divd = fn (x,y) => Op ("divd", [x,y], Ty.Double)
val ltd = fn (x,y) => Op ("ltd", [x,y], Ty.Double)
val leqd = fn (x,y) => Op ("leqd", [x,y], Ty.Double)
val eqd = fn (x,y) => Op ("eqd", [x,y], Ty.Double)
val maxd = fn x => fn y => Op ("maxd", [x,y], Ty.Double)
val mind = fn x => fn y => Op ("mind", [x,y], Ty.Double)
val negd = fn x => Op ("negd", [x], Ty.Double)

val i2d = fn x => Op ("i2d", [x], Ty.Double)
val op % = fn (x,y) => Op ("mod", [x,y], Ty.Int)

fun Op'(a,b) = Op(a,b,TyVar())
fun Fn'(v,t,e) = Fn(v,t,e,TyVar())     
fun Vc' e = Vc(e,TyVar())
fun If (b,e1,e2) = Iff(b,e1,e2,TyVar())

val fromList = fn _ => Vc'
val op + = fn (x,y) => Op' ("add", [x,y])
val op - = fn (x,y) => Op' ("sub", [x,y])
val op * = fn (x,y) => Op' ("mul", [x,y])
val op / = fn (x,y) => Op' ("div", [x,y])
val op < = fn (x,y) => Op' ("lt", [x,y])
val op <= = fn (x,y) => Op' ("leq", [x,y])
val op == = fn (x,y) => Op' ("eq", [x,y])
val max = fn x => fn y => Op' ("max", [x,y])
val min = fn x => fn y => Op' ("min", [x,y])
val ~ = fn x => Op' ("neg", [x])

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

(* Compiled Programs *)
type ('a,'b) prog = exp
fun runM _ m = m (fn x => x)
fun runF _ f = f (Var("arg",TyVar())) (fn x => x)
fun outprog outfile e = raise Fail "outprog:unimplemented"
 
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
type 'a m = exp * int
fun zilde _ = (Op'("zilde",nil),1)
fun scl _ t = (t,0)
fun vec t = (t,1) (*Op'("vec",[t])*)
fun iota t = (Op'("iota",[t]),1)
fun siz (t,_) = Op'("siz",[t])
fun dim (t,_) = Op'("dim",[t])   (* or I(#2 t) *)
fun rav (t,_) = (Op'("rav",[t]),1)
fun rav0 (t,_) = t
fun each t _ f (e,r) =
    let val v = newVar()
        val e0 = f (Var(v,TyVar())) (fn x => x)
    in (Op'("each",[Fn'(v,t,e0),e]),r) 
    end
fun mkFn2 t1 t2 f =
    let val (v1, v2) = (newVar(), newVar())
        val e0 = f (Var(v1,t1), Var(v2,t2))
    in Fn'(v1,t1,Fn'(v2,t2,e0))
    end
fun mkFn2m t1 t2 f = mkFn2 t1 t2 (fn a => f a (fn x=>x))
fun bin t1 t2 s f e1 e2 =
    ret(Op'(s,[mkFn2 t1 t2 f,e1,e2]))
fun binm t1 t2 s f e1 e2 = 
    bin t1 t2 s (fn a => f a (fn x => x)) e1 e2
fun red t1 t2 f n (e,_) = binm t1 t2 "red" f n e
fun meq t f (e1,_) (e2,_) = bin t t "meq" f e1 e2
fun mif (b,(e1,r1),(e2,r2)) = if r1 = r2 then (If(b,e1,e2),r1)
                              else raise Fail "rank error: mif"
fun sum t1 t2 _ f (e1,r1) (e2,r2) =
    if r1 = r2 then binm t1 t2 "sum" f e1 e2 >>= (fn e => ret(e, r1))
    else raise Fail "rank error: sum"
fun scan t1 t2 f e1 (e2,r) = bin t1 t2 "scan" f e1 e2 >>= (fn e => ret(e,r))
fun catenate (e1,r1) (e2,r2) =
    if r1 = 0 andalso r2 = 0 then ret(Vc'[e1,e2],1)
    else if r1 = r2 then ret(Op'("catenate", [e1,e2]),r1)
    else if r1 = Int.+(r2,1) then ret(Op'("snoc",[e1,e2]),r1)
    else if r2 = Int.+(r1,1) then ret(Op'("cons",[e1,e2]),r2)
    else raise Fail "rank error: catenate"
fun take e1 (e2,r) = (Op'("take", [e1,e2]),r)
fun drop e1 (e2,r) = (Op'("drop", [e1,e2]),r)
fun mem (e,r) = ret(Op'("mem",[e]),r)
fun rotate e1 (e2,r) = (Op'("rotate", [e1,e2]),r)
fun reshape e1 (e2,_) = 
    let val r = vecLength e1
    in ret(Op'("reshape", [e1,e2]),r)
    end
fun shape (e,_) = Op'("shape",[e])
fun prod t f g e (m1,r1) (m2,r2) s a =
    let open Int
        val r = if r1 < 1 orelse r2 < 1 then raise Fail "rank error: prod"
                else r1 + r2 - 2
        val res = Op'("prod",[mkFn2m t t f,mkFn2m t t g,e,m1,m2])
    in if r = 0 then ret(s res)
       else ret(a(res,r))
    end
fun reduce t f e1 (e2,r) s a =
    let open Int
    in binm t t "reduce" f e1 e2 >>= 
            (fn e => ret(if r < 1 then raise Fail "rank error: reduce"
                         else if r > 1  then a(e,r-1) else s e))
    end
fun transpose (e,r) = (Op'("transpose", [e]),r)

fun lett ty e = 
    let val v = newVar()
    in fn f => Let(v,ty,e,f (Var(v,ty)),TyVar())
    end

fun letm ty (e,r) = 
    let val v = newVar()   
        val tv = TyVar()   (* memo: why this tyvar? *)
    in fn f => Let(v,tv,e,f (Var(v,tv),r),TyVar())
    end

fun pp_exp e =
    let infix @@
        fun isVars nil nil = true
          | isVars (v::vs) (Var(v',_)::es) = v=v' andalso isVars vs es
          | isVars _ _ = false
        fun lookForOp vs e =
            case e of
                Fn(v,_,e',_) => lookForOp (v::vs) e'
              | Op(opr,es,_) => if isVars (rev vs) es then SOME opr
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
                | Op (opr,nil,_) => $opr
                | Op (opr,es,_) => $opr @@ $"(" @@ pps (i+1+size opr) es @@ $")"
                | Let (v,ty,e1,e2,_) => $"let " @@ $v @@ $":" @@ $(prType ty) @@ $" = " @@ pp (i+2) e1 @@
                                         indent i @@ $"in " @@ pp (i+3) e2 @@ 
                                         indent i @@ $"end"
                | Fn (v,t,e,_) =>
                  (case lookForOp [v] e of
                       SOME opr => $opr
                     | NONE => $("fn " ^ v ^ ":" ^ prType t ^ " => ") @@ pp (i+2) e)
        and pps i nil = $""
          | pps i [e] = pp i e
          | pps i (e::es) = pp i e @@ $"," @@ pps i es
    in flatten(pp 0 e)
    end

val pp_prog = pp_exp
val ppV = Exp.pr_value

fun eval p v =
    let val () = print ("Untyped program:\n" ^ pp_prog p ^ "\n")
        val () = print ("Typing the program...\n")
    in case typeExp empEnv p of
           ERR s => raise Fail ("***Type error: " ^ s)
         | OK t => (print ("  Program has type: " ^ prType t ^ "\n");
                    print ("Typed program:\n" ^ pp_prog p ^ "\n");
                    print ("Evaluating program\n");
                    let val de = Exp.addDE Exp.empDEnv "arg" v
                        val v' = Exp.eval de p
                    in v'
                    end)
    end
    
end
