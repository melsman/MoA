signature EXP_TYPE = sig
  type rnk
  val rnk       : int -> rnk
  val unRnk     : rnk -> int option
  val RnkVar    : unit -> rnk
  val RnkVarCon : (int->string option) -> rnk
  val relateR   : (int -> int) * (int -> int) -> rnk -> rnk -> string option
  val relateR2  : {f12: int*int->int,f13:int*int->int,f23:int*int->int} -> rnk -> rnk -> rnk -> string option
  val unifyR    : rnk -> rnk -> string option
  val prRnk     : rnk -> string

  type typ
  type bty
  val IntB     : bty
  val BoolB    : bty
  val DoubleB  : bty
  val Int      : typ
  val isInt    : bty -> bool
  val isDouble : bty -> bool
  val isBool   : bty -> bool
  val Bool     : typ
  val Double   : typ
  val Sh       : rnk -> typ
  val Arr      : bty -> rnk -> typ
  val Vec      : typ -> typ                (* assert argument is a scalar type *)
  val unArr    : typ -> (bty * rnk) option
  val Fun      : typ * typ -> typ
  val unFun    : typ -> (typ * typ) option
  val TyVar    : unit -> typ
  val TyVarB   : unit -> bty
  val unify    : typ -> typ -> string option
  val unifyB   : bty -> bty -> string option
  val prType   : typ -> string
  val prBty    : bty -> string

  type var = string
  type opr = string
end

signature EXP = sig
  
  structure T : EXP_TYPE

  type rnk = T.rnk
  type typ = T.typ
  type var = T.var
  type opr = T.opr
  datatype exp =
           Var of var * typ
         | I of int
         | D of real
         | B of bool
         | Iff of exp * exp * exp * typ
         | Vc of exp list * typ
         | Op of opr * exp list * typ
         | Let of var * typ * exp * exp * typ
         | Fn of var * typ * exp * typ

  type env
  val lookup   : env -> var -> typ option
  val empEnv   : env
  val add      : env -> var -> typ -> env

  datatype 't report = OK of 't | ERR of string
  val typeExp  : env -> exp -> typ report

  type value
  type denv
  val empDEnv  : denv
  val addDE    : denv -> var -> value -> denv
  val pr_value : value -> string
  val Dvalue   : real -> value
  val unDvalue : value -> real
  val Uvalue   : value

  val eval : denv -> exp -> value
  
end

functor Exp(T : EXP_TYPE) : EXP = struct
  structure T = T
  open T

  (* General feature functions *)
  fun qq s = "'" ^ s ^ "'" 
  fun curry f x y = f (x,y)
  fun isIn s xs = List.exists (curry (op =) s) xs
                           
  (* Some type utilities *)
  fun unScl s t =
      case unArr t of
          SOME (bt,r) => (case unifyR r (rnk 0) of
                              SOME s' => raise Fail (s' ^ " in " ^ s)
                            | NONE => bt)
        | NONE =>
          let val bt = TyVarB()
          in case unify t (Arr bt (rnk 0)) of
                 SOME s' => raise Fail (s' ^ " in " ^ s)
               | NONE => bt
          end

  fun unBinFun s ty =
      let fun err t = 
              raise Fail ("expected function type, but got " ^ prType t)
      in case unFun ty of
             SOME (t1,t) =>
             (case unFun t of
                  SOME(t2,t3) => (unScl "first function argument" t1,
                                  unScl "second function argument" t2,
                                  unScl "function result" t3)
                | NONE => err t)
           | NONE => err ty
      end

  fun unArr' s t =
      case unArr t of
          SOME p => p
        | NONE =>
          let val tv = TyVarB()
              val r = RnkVar()
          in case unify t (Arr tv r) of
                 NONE => (tv,r)
               | SOME _ => 
                 raise Fail ("expecting array type, but got "
                             ^ prType t ^ " in " ^ s)
          end

  fun unFun' s t =
      case unFun t of
          SOME (t1,t2) => (unScl s t1, unScl s t2)
        | NONE => raise Fail ("expecting function type, but got "
                              ^ prType t ^ " in " ^ s)

  (* Expressions *)

  datatype exp =
           Var of var * typ
         | I of int
         | D of real
         | B of bool
         | Iff of exp * exp * exp * typ
         | Vc of exp list * typ
         | Op of opr * exp list * typ
         | Let of var * typ * exp * exp * typ
         | Fn of var * typ * exp * typ

  (* Environments *)
  type env = (var * typ) list

  val empEnv = nil

  fun lookup e v =
      case e of
          nil => NONE
        | (x,t)::e => if x = v then SOME t
                      else lookup e v

  fun add e v t = (v,t)::e

  (* Typing *)

  fun assert0 unify s t1 t2 =
      case unify t1 t2 of
          SOME s' => raise Fail (s' ^ " in " ^ s)
        | NONE => ()

  val assert = assert0 unify
  val assertR = assert0 unifyR
  val assertB = assert0 unifyB
  
  fun isBinOpIII opr =
      isIn opr ["addi","subi","muli","divi","maxi","mini","mod"]

  fun isBinOpDDD opr =
      isIn opr ["addd","subd","muld","divd","maxd","mind"]

  fun isBinOpIIB opr =
      isIn opr ["lti","leqi","eqi"]

  fun isBinOpDDB opr =
      isIn opr ["ltd","leqd","eqd"]

  fun conssnoc opr ty E e1 e2 =
      let val t2 = ty E e2
          val (b1,r1) = unArr' ("first argument to " ^ opr) (ty E e1)
          val (b2,r2) = unArr' ("second argument to " ^ opr) t2
          val rv = RnkVarCon (fn i => unifyR r2 (rnk(i+1)))
      in assertR ("arguments to " ^ opr) rv r1
       ; assertB ("arguments to " ^ opr) b1 b2
       ; t2
      end

  datatype 't report = OK of 't | ERR of string
  fun typeExp (E:env) e : typ report =
      let fun ty E e =
              case e of
                  Var(v, t0) => (case lookup E v of
                                     SOME t => (assert "var" t t0; t)
                                  |  NONE => raise Fail ("Unknown variable " ^ qq v))
                | I _ => Int
                | D _ => Double
                | B _ => Bool
                | Iff (c,e1,e2,t0) =>
                  let val t1 = ty E e1
                  in assert "if" t1 t0
                   ; assert "conditional expression" Bool (ty E c)
                   ; assert "else-branch" t1 (ty E e2)
                   ; t1
                  end
                | Vc(nil,t0) =>
                  let val t = Arr (TyVarB()) (rnk 1)
                  in assert "empty vector" t t0
                   ; t
                  end
                | Vc (e::es,t0) =>
                  let val t = ty E e
                      val (b,r) = unArr' "vector expression" t
                      val () = assertR "vector expression" r (rnk 0)
                      val () = List.app (fn e => assert "vector element" t (ty E e)) es
                      val t = if isInt b then Sh(rnk(1+length es))  (* shape type *)
                              else Arr b (rnk 1)                    (* vector type *)
                  in assert "vector" t t0
                   ; t
                  end
                | Let (v,t,e1,e2,t0) =>
                  (assert ("let-binding of " ^ v) t (ty E e1);
                   let val t' = ty (add E v t) e2
                   in assert "let" t' t0
                    ; t'
                   end)
                | Fn (v,t,e,t0) =>
                  let val t' = ty (add E v t) e
                  in assert "fun" t' t0;
                     Fun(t,t')
                  end
                | Op (opr, es, t0) => 
                  let val t = tyOp E opr es
                  in assert opr t t0
                   ; t
                  end
          and tyOp E opr es =
              case (opr, es) of
                  ("zilde", nil) => Arr (TyVarB()) (rnk 1)
                | ("iota", [e]) => 
                  (assert "iota expression" Int (ty E e);
                   Arr IntB (rnk 1))  (* memo: could be shape! *)
                | ("shape",[e]) =>
                  let val (_,r) = unArr' "shape argument" (ty E e)
                  in Sh r
                  end
                | ("reshape",[e1,e2]) =>
                  let val t1 = ty E e1
                      val t2 = ty E e2
                      val (bt,_) = unArr' "second argument to reshape" t2
                      val r = RnkVar()
                  in assert "first argument to reshape" (Sh r) t1;
                     Arr bt r
                  end
                | ("take",[e1,e2]) =>
                  let val t1 = ty E e1
                  in assert "first argument to take" Int t1;
                     ty E e2
                  end
                | ("drop",[e1,e2]) =>
                  let val t1 = ty E e1
                  in assert "first argument to take" Int t1;
                     ty E e2
                  end
                | ("catenate",[e1,e2]) =>
                  let val t1 = ty E e1
                      val t2 = ty E e2
                  in assert "arguments to catenate" t1 t2
                   ; t1
                  end
                | ("cons",[e1,e2]) => conssnoc opr ty E e1 e2
                | ("snoc",[e1,e2]) => conssnoc opr ty E e2 e1
                | ("transpose",[e]) => ty E e 
                | ("sum",[f,e1,e2]) =>
                  let val (bt1,bt2,bt) = unBinFun "first argument to sum" (ty E f)
                      val (bt1',r1) = unArr' "sum first argument" (ty E e1)
                      val (bt2',r2) = unArr' "sum second argument" (ty E e2)
                  in assertB "first argument to sum" bt1 bt1'
                   ; assertB "second argument to sum" bt2 bt2'
                   ; assertR "sum argument ranks" r1 r2
                   ; Arr bt r1
                  end
                | ("reduce", [f,n,v]) =>
                  let val (bt1,bt2,bt) = unBinFun "first argument to reduce" (ty E f)
                      val btn = unScl "reduce neutral element" (ty E n)
                      val (btv,r) = unArr' "reduce argument" (ty E v)
                      val () = List.app (assertB "reduce function" btn) [bt1,bt2,bt,btv]
                      val rv = RnkVarCon (fn i => unifyR r (rnk(i+1)))
                      val rv' = RnkVarCon (fn i => unifyR rv (rnk(i-1)))
                  in assertR "reduce" rv' r
                   ; Arr bt rv
                  end
                | ("each", [f,v]) =>
                  let val (bt,r) = unArr' "each" (ty E v)
                      val (bt1,bt2) = unFun' "first argument to each" (ty E f)
                  in assertB "each elements" bt1 bt;
                     Arr bt2 r
                  end
                | ("prod",[f,g,n,v1,v2]) =>
                  let val t = unScl "prod neutral element" (ty E n)
                      val (f1,f2,f3) = unBinFun "first argument to prod" (ty E f)
                      val (g1,g2,g3) = unBinFun "second argument to prod" (ty E g)
                      val (v1t,r1) = unArr' "prod" (ty E v1)
                      val (v2t,r2) = unArr' "prod" (ty E v2)
                      val () = List.app (fn (t1,t2) => assertB "prod" t1 t2) 
                                        [(f1,f2),(f2,f3),(f3,t),
                                         (g1,g2),(g2,g3),(g3,t),
                                         (v1t,v2t),(v2t,t)]
                      val rv = RnkVar()
                      val rv1 = RnkVarCon (fn i1 =>
                                              let val rv2 = RnkVarCon (fn i2 =>
                                                                          let val r = i1 + i2 - 2
                                                                          in if r < 0 then SOME "Negative rank for prod"
                                                                             else unifyR rv (rnk r)
                                                                          end)
                                              in unifyR rv2 r2
                                              end)
                  in assertR "rank for prod" r1 rv1;
                     Arr t rv
                  end
                | ("i2d",[e]) => (assert opr Int (ty E e); Double)
                | ("negi",[e]) => (assert opr Int (ty E e); Int)
                | ("negd",[e]) => (assert opr Double (ty E e); Double)
                | (_,[e1,e2]) => 
                  if isBinOpIII opr then tyBin Int Int Int opr E e1 e2
                  else if isBinOpDDD opr then tyBin Double Double Double opr E e1 e2
                  else if isBinOpIIB opr then tyBin Int Int Bool opr E e1 e2
                  else if isBinOpDDB opr then tyBin Double Double Bool opr E e1 e2
                  else raise Fail ("binary operator " ^ qq opr ^ " not supported")
                | _ => raise Fail ("operator " ^ qq opr ^ ", with " 
                                   ^ Int.toString (length es) 
                                   ^ " arguments, not supported")
          and tyBin t1 t2 t3 opr E e1 e2 =
             (assert ("first argument to " ^ opr) t1 (ty E e1);
              assert ("second argument to " ^ opr) t2 (ty E e2);
              t3)
      in OK(ty E e) handle Fail s => ERR s
      end

  datatype bv = Ib of int
              | Db of real
              | Bb of bool
              | Fb of denv * var * typ * exp * typ
  withtype denv = (var * bv Apl.t) list

  type value = bv Apl.t

  fun Dvalue v = Apl.scl (Db v)
  fun unDvalue _ = raise Fail "exp.unDvalue: not implemented"
  val Uvalue = Dvalue 0.0

  fun pr_double d =
      if d < 0.0 then "-" ^ pr_double (~d)
      else if Real.==(d,Real.posInf) then "HUGE_VAL"
      else let val s = Real.toString d
           in if CharVector.exists (fn c => c = #".") s then s
              else s ^ ".0"
           end

  fun pr_bv b =
      case b of
          Ib b => Int.toString b
        | Db b => pr_double b
        | Bb b => Bool.toString b
        | Fb _ => "fn"
                      
  fun pr_value v = Apl.pr pr_bv v

  val empDEnv = nil
  val addDE = add

  fun unIb (Ib b) = b
    | unIb _ = raise Fail "exp.unIb"
  fun unDb (Db b) = b
    | unDb _ = raise Fail "exp.unDb"
  fun unBb (Bb b) = b
    | unBb _ = raise Fail "exp.unBb"
  fun unFb (Fb b) = b
    | unFb _ = raise Fail "exp.unFb"


  fun unBase s t fi fd fb =
      let val (bt,_) = unArr' ("unBase:" ^ s) t
      in if isInt bt then fi()
         else if isDouble bt then fd()
         else if isBool bt then fb()
         else raise Fail ("exp.unBase: expecting base type: " ^ s)
      end

  fun default t =
      unBase "default" t (fn() => Ib 0) (fn() => Db 0.0) (fn() => Bb true)

  fun resType (_,_,_,_,_,_,t) = t

  fun eval DE e : value =
      case e of
          Var(x,_) =>
          (case lookup DE x of
               SOME v => v
             | NONE => raise Fail ("eval.cannot locate variable " ^ x))
        | I i => Apl.scl (Ib i)
        | D d => Apl.scl (Db d)
        | B b => Apl.scl (Bb b)
        | Iff (e1,e2,e3,t) =>
          let val b = Apl.liftU (fn Bb b => b | _ => raise Fail "eval:Iff") (eval DE e1)
          in Apl.iff(b,fn() => eval DE e2, fn() => eval DE e3)
          end
        | Vc (nil,t) => Apl.zilde (default t)
        | Vc (x::xs,t) => eval DE (Op("cons",[x, Vc(xs,t)],t))
        | Op (opr, es, t) =>
          let fun fail() = raise Fail ("exp.eval: operator " ^ opr ^ " not supported with " 
                                       ^ Int.toString (length es) ^ " arguments")
          in case (opr,es) of
                 ("zilde", []) => Apl.zilde (default t)
               | ("i2d", [e]) => Apl.liftU (fn Ib i => Db(real i) | _ => raise Fail "eval:i2d") (eval DE e)
               | ("iota", [e]) => Apl.map Ib (Apl.iota (Apl.map unIb (eval DE e)))
               | ("reshape", [e1,e2]) =>
                 let val v1 = Apl.map unIb (eval DE e1)
                 in Apl.reshape(v1,eval DE e2)
                 end
               | ("shape", [e]) =>
                 let val v = Apl.shape(eval DE e)
                 in Apl.map Ib v
                 end
               | ("drop", [e1,e2]) =>
                 let val v1 = Apl.map unIb (eval DE e1)
                 in Apl.drop(v1,eval DE e2)
                 end
               | ("take", [e1,e2]) =>
                 let val v1 = Apl.map unIb (eval DE e1)
                 in Apl.take(v1,eval DE e2)
                 end
               | ("transpose", [e]) => Apl.transpose (eval DE e)
               | ("cons", [e1,e2]) => Apl.cons(eval DE e1,eval DE e2)
               | ("snoc", [e1,e2]) => Apl.snoc(eval DE e1,eval DE e2)
               | ("catenate", [e1,e2]) => Apl.catenate(eval DE e1,eval DE e2)
               | ("reduce", [f,n,a]) =>
                 let val F = unFb2 DE "reduce" f
                     val n = eval DE n
                     val a = eval DE a
                 in Apl.reduce (applyBin F) n a
                 end
               | ("each", [e1,e2]) =>
                 let val (DE0,v,t,e,t') = unFb(Apl.unScl"eval:each"(eval DE e1))
                 in Apl.each (default t') (fn y => eval (addDE DE0 v y) e) (eval DE e2)
                 end
               | ("sum", [f,e2,e3]) =>
                 let val F = unFb2 DE "sum" f
                 in Apl.zipWith (default (resType F)) (applyBin F) (eval DE e2) (eval DE e3)
                 end
               | ("prod",[f,g,n,v1,v2]) =>
                 let val F = unFb2 DE "prod first" f
                     val G = unFb2 DE "prod second" g
                     val n = eval DE n
                     val v1 = eval DE v1
                     val v2 = eval DE v2
                 in Apl.dot (applyBin F) (applyBin G) n v1 v2
                 end
               | (opr,[e1,e2]) =>
                 let val v1 = eval DE e1
                     val v2 = eval DE e2
                 in if isBinOpIII opr then evalBinOpIII opr v1 v2
                    else if isBinOpDDD opr then evalBinOpDDD opr v1 v2
                    else if isBinOpIIB opr then evalBinOpIIB opr v1 v2
                    else if isBinOpDDB opr then evalBinOpDDB opr v1 v2
                    else fail()
                 end
               | (opr, _) => fail()
          end
        | Let (v,t,e1,e2,t') => eval (addDE DE v (eval DE e1)) e2
        | Fn (v,t,e,t') => Apl.scl (Fb(DE,v,t,e,t'))
  and unFb2 DE s e =
      let val (DE0,x,tx,e,_) = unFb(Apl.unScl (s ^ ", first function argument") (eval DE e))
          val (_,y,ty,e,t) = unFb(Apl.unScl (s ^ ", second function argument") (eval DE e))
      in (DE0,x,y,e,tx,ty,t)
      end
  and applyBin (DE0,x,y,e,_,_,_) (a,b) =
      let val DE0 = addDE DE0 x a
          val DE0 = addDE DE0 y b
      in eval DE0 e
      end
  and evalBinOpIII opr v1 v2 =
      let val fct = case opr of
                        "addi" => (op +)
                      | "subi" => (op -)
                      | "muli" => (op * )
                      | "divi" => (op div)
                      | "maxi" => (fn (x,y) => if x > y then x else y)
                      | "mini" => (fn (x,y) => if x < y then x else y)
                      | "mod" => (op mod)
                      | _ => raise Fail ("evalBinOpIII: unsupported int*int->int operator " ^ opr)
      in Apl.liftB (fn (b1,b2) => Ib(fct(unIb b1, unIb b2))) (v1,v2)
      end
  and evalBinOpIIB opr v1 v2 =
      let val fct = case opr of
                        "lti" => (op <)
                      | "leqi" => (op <=)
                      | "eqi" => (op =)
                      | _ => raise Fail ("evalBinOpIIB: unsupported int*int->bool operator " ^ opr)
      in Apl.liftB (fn (b1,b2) => Bb(fct(unIb b1, unIb b2))) (v1,v2)
      end
  and evalBinOpDDD opr v1 v2 =
      let val fct = case opr of
                        "addd" => (op +)
                      | "subd" => (op -)
                      | "muld" => (op * )
                      | "divd" => (op /)
                      | "maxd" => (fn (x,y) => if x > y then x else y)
                      | "mind" => (fn (x,y) => if x < y then x else y)
                      | _ => raise Fail ("evalBinOpDDD: unsupported double*double->double operator " ^ opr)
      in Apl.liftB (fn (b1,b2) => Db(fct(unDb b1, unDb b2))) (v1,v2)
      end
  and evalBinOpDDB opr v1 v2 =
      let val fct = case opr of
                        "lti" => (op <)
                      | "leqi" => (op <=)
                      | "eqi" => Real.==
                      | _ => raise Fail ("evalBinOpDDB: unsupported double*double->bool operator " ^ opr)
      in Apl.liftB (fn (b1,b2) => Bb(fct(unDb b1, unDb b2))) (v1,v2)
      end
end
