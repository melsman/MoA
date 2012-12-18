structure IL = struct

datatype Type = Int | Double | Bool | Vec of Type

structure Name : sig
  type t
  val new : Type -> t
  val pr : t -> string
  val typeOf : t -> Type
end =
struct
  type t = string * Type
  val count = ref 0
  fun new t = ("n" ^ Int.toString(!count) before count := !count + 1, t)
  fun pr (s,_) = s
  fun typeOf (_,t) = t
end

structure NameSet = OrderSet(struct type t = Name.t
                                    fun compare (x, y) = String.compare (Name.pr x, Name.pr y)
                             end)

datatype Value =
         IntV of int
       | DoubleV of real
       | BoolV of bool                  
       | ArrV of Value option ref vector
datatype Unop = Neg | I2D | D2I
datatype Binop = Add | Sub | Mul | Divv | Modv | Min | Max | Lt | Lteq | Eq
datatype Exp =
         Var of Name.t
       | I of int
       | D of real
       | T | F
       | If of Exp * Exp * Exp
       | Subs of Name.t * Exp
       | Alloc of Type * Exp
       | Binop of Binop * Exp * Exp
       | Unop of Unop * Exp
                
type Size = Exp
type Index = Exp
             
datatype Stmt =
         For of Exp * (Exp -> Stmt list)
       | Assign of Name.t * Exp
       | AssignArr of Name.t * Exp * Exp
       | Decl of Name.t * Exp
       | Nop
       | Free of Name.t
       | Ret of Exp

fun eq(e1,e2) =
    case (e1, e2) of
      (Var n1, Var n2) => n1 = n2
    | (I i1, I i2) => i1 = i2
    | (D d1, D d2) => Real.==(d1, d2)
    | (T,T) => true
    | (F,F) => true
    | (If(a1,a2,a3), If(b1,b2,b3)) => eq(a1,b1) andalso eq(a2,b2) andalso eq(a3,b3)
    | (Subs(n1,a),Subs(n2,b)) => n1=n2 andalso eq(a,b)
    | (Alloc(t1,a),Alloc(t2,b)) => t1 = t2 andalso eq(a,b)
    | (Binop(p1,a1,a2),Binop(p2,b1,b2)) => p1=p2 andalso eq(a1,b1) andalso eq(a2,b2)
    | (Unop(p1,a1),Unop(p2,b1)) => p1=p2 andalso eq(a1,b1)
    | _ => false
end

signature TYPE = sig
  eqtype Int and Double and 'a Num  (* numeric types *)
     and Bool                       (* booleans *)
     and 'a Vec                     (* vectors *)

  eqtype 'a T                       (* Type constructors *)
  val Int      : Int Num T
  val Double   : Double Num T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
  val prType   : 'a T -> string
  val vecElem  : 'a Vec T -> 'a T
end

structure Type : TYPE = struct
  type Int = unit
   and Double = unit
   and 'a Num = unit
   and Bool = unit
   and 'a Vec = unit

  type 'a T = IL.Type
  val Int = IL.Int
  val Double = IL.Double
  val Bool = IL.Bool
  val Vec = IL.Vec
  fun prType IL.Int = "int"
    | prType IL.Double = "double"
    | prType IL.Bool = "bool"
    | prType (IL.Vec t) = prType t ^ "[]"
  fun vecElem (IL.Vec t) = t
    | vecElem t = raise Fail ("vecElem: Expecting vector type - got " ^ prType t)
end

signature NAME = sig
  eqtype t
  val new : 'a Type.T -> t
  val pr  : t -> string
end

structure Name : NAME = struct
  open IL.Name
end

signature PROGRAM = sig
  type e
  val $     : Name.t -> e
  val Subs  : Name.t * e -> e
  val Alloc : 'a Type.T * e -> e
  val I     : int -> e
  val D     : real -> e
  val B     : bool -> e
  val If    : e * e * e -> e
  val +     : e * e -> e
  val -     : e * e -> e
  val *     : e * e -> e
  val /     : e * e -> e
  val %     : e * e -> e
  val <     : e * e -> e
  val <=    : e * e -> e
  val ==    : e * e -> e
  val ~     : e -> e
  val i2d   : e -> e
  val d2i   : e -> e
  val max   : e -> e -> e
  val min   : e -> e -> e
  val unI   : e -> int option
  val unD   : e -> real option

  type s
  type ss = s list
  val For : e * (e -> ss) -> ss -> ss
  val :=  : Name.t * e -> s
  val ::= : (Name.t * e) * e -> s
  val Decl: Name.t * e -> s
  val Ret : e -> s
  val Free : Name.t -> s
  val emp : s
  val unDecl : s -> (Name.t * e) option

  (* static evaluation *)
  datatype info = EqI of e
                | LtI of e
                | GtEqI of e 
  type env = (Name.t * info) list
  val se_e  : env -> e -> e
  val se_ss : env -> ss -> ss * env
end

structure Program : PROGRAM = struct
local open IL
in
  type e = Exp

  fun closed e =
      case e of
         Var _ => false
       | I _ => true
       | D _ => true
       | T => true
       | F => true
       | If(e0,e1,e2) => closed e0 andalso closed e1 andalso closed e2
       | Subs (_,e) => closed e
       | Alloc (_,e) => closed e
       | Binop(_,e1,e2) => closed e1 andalso closed e2
       | Unop(_,e) => closed e

  structure N = NameSet
  fun uses e acc =
      case e of
         Var n => N.insert (acc,n)
       | I _ => acc
       | D _ => acc
       | T => acc
       | F => acc
       | If(e0,e1,e2) => uses e0 (uses e1 (uses e2 acc))
       | Subs (_,e) => uses e acc
       | Alloc (_,e) => uses e acc
       | Binop(_,e1,e2) => uses e1 (uses e2 acc)
       | Unop(_,e) => uses e acc

  fun uses_s s =
      case s of
        IL.Assign (_,e) => uses e N.empty
      | IL.AssignArr (n,e1,e2) => uses e1 (uses e2 (N.singleton n))
      | IL.Decl (_,e) => uses e (N.empty)
      | IL.Nop => N.empty
      | IL.Free n => N.singleton n
      | IL.Ret e => uses e N.empty
      | IL.For (e,f) => raise Fail "uses_s"

  fun defs_s s =
      case s of
        IL.Nop => N.empty
      | IL.Ret e => N.empty
      | IL.Free n => N.empty
      | IL.Decl(n,e) => N.singleton n
      | IL.Assign(n,e) => N.singleton n
      | IL.AssignArr(n,e0,e) => N.empty
      | IL.For(e,f) => raise Fail "defs_s"
(*
  fun dce ss =
      case ss of
        nil => (nil,N.empty)
      | s :: ss =>
        let val (ss',U) = dce ss
        in case s of
             IL.For(e,f) =>
             let val n = Name.new Type.Int
                 val body = f($n)
             in
             end
           | _ =>
             let val D = defs_s s
             in if N.isEmpty (N.intersect(D,U)) then
                  (ss',U)
                else
                  (s :: ss', N.union(uses_s s,N.difference(U,D))
             end
        end
*)

  fun nevereq(a,b) =
      case (a, b) of
        (IL.T,IL.F) => true
      | (IL.I x,IL.I y) => x <> y
      | (IL.D x,IL.D y) => not(Real.==(x,y))
      | _ => false

  fun If(IL.T,b,c) = b
    | If(IL.F,b,c) = c
    | If(a,b,c) = 
      let fun default() = 
              if IL.eq(b,c) then b 
              else if IL.eq(b, IL.T) andalso IL.eq(a,c) then a
              else if IL.eq(c, IL.F) andalso IL.eq(a,b) then a
              else if IL.eq(b, IL.F) andalso IL.eq(a,c) then IL.F
              else if IL.eq(c, IL.T) andalso IL.eq(a,b) then IL.T
              else 
                case c of
                  IL.If(d,e,f) => if eq(a,d) then If(a,b,f) else IL.If(a,b,c)
                | _ => IL.If(a,b,c)
      in case a of
           IL.Binop(Eq,IL.If(x,y,z),e) => 
           (* If(If(x,y,z)==y,s,t) => If(x,s,t) and S(z)<>S(y), for all S *)
           if eq(y,e) andalso nevereq(z,y) then If(x,b,c)
           else default()
         | IL.Binop(Eq,c',b') => if eq(c,c') andalso eq(b,b') then c 
                                 else default()
         | _ => default()
      end

  fun $ n = Var n
  fun I n = IL.I n
  fun unI (IL.I n) = SOME n
    | unI _ = NONE
  fun D n = IL.D n
  fun unD (IL.D n) = SOME n
    | unD _ = NONE
  fun B true = T
    | B false = F 

  fun comp0 neg t acc =
      case t of
        IL.I i => (if neg then Int.-(acc,i) else Int.+(acc,i), fn x => x)
      | Binop(Add,a,b) => 
        let val (acc, f) = comp0 neg a acc
            val (acc, g) = comp0 neg b acc
        in (acc, f o g)
        end
      | Binop(Sub,a,b) =>
        let val (acc, f) = comp0 neg a acc
            val (acc, g) = comp0 (not neg) b acc
        in (acc, f o g)
        end        
      | Var _ => (acc, fn x => Binop(if neg then Sub else Add, x, t))
      | _ => raise Fail "no"

  and comp p a b =
      let val t = Binop(p,a,b)
      in let val (acc, f) = comp0 false t 0
         in f (I acc)
         end handle Fail _ => t
      end

  and a        - (IL.I 0) = a
    | (IL.I a) - (IL.I b) = I(Int.-(a,b))
    | (IL.D a) - (IL.D b) = D(Real.-(a,b))
    | (Binop(Sub,IL.I a,b)) - (IL.I c) = I(Int.-(a,c)) - b 
    | (Binop(Sub,a,IL.I b)) - (IL.I c) = a - I(Int.+(b,c)) 
    | (IL.If(x,y,z)) - (IL.I a) = If(x,y-(I a),z-(I a))
    | (Binop(Add,a,IL.I b)) - (IL.I c) = a + (I (Int.-(b,c)))
    | (Binop(Add,IL.I a,b)) - (IL.I c) = I (Int.-(a,c)) + b
    | a        - b        = comp Sub a b

  and (IL.I 0)              + b       = b
    | a                     + (IL.I 0) = a
    | (IL.I a)              + (IL.I b) = I(Int.+(a,b))
    | (IL.D a)              + (IL.D b) = D(Real.+(a,b))
    | (Binop(Sub,a,IL.I b)) + (IL.I c) = let val d = Int.-(c,b)
                                         in if d > 0 then a + (I d)
                                            else a - I (~d)
                                         end
    | (Binop(Sub,IL.I a,b)) + (IL.I c) = I(Int.+(a,c)) - b
    | (Binop(Add,x,IL.I a)) + (IL.I b) = x + I (Int.+(a,b))
    | (Binop(Add,IL.I a,x)) + (IL.I b) = x + I (Int.+(a,b))
    | (IL.I b) + (Binop(Add,IL.I a,x)) = x + I (Int.+(a,b))
    | (IL.I b) + (Binop(Add,x,IL.I a)) = x + I (Int.+(a,b))
    | (IL.If(x,y,z))        + (IL.I a) = If(x,y+(I a),z+(I a))
    | (IL.I a)              + (IL.If(x,y,z)) = If(x,y+(I a),z+(I a))
    | (a as IL.If(e,x,y))   + (b as IL.If(e',x',y')) = if eq(e,e') then If(e,x+x',y+y')
                                                       else comp Add a b
    | a                    + b         = comp Add a b

  fun (IL.I a) * (IL.I b) = I(Int.*(a,b))
    | (IL.D a) * (IL.D b) = D(Real.*(a,b))
    | (IL.I 1) * b        = b
    | a        * (IL.I 1) = a
    | (IL.I 0) * b        = I 0
    | a        * (IL.I 0) = I 0
    | a        * b        = Binop(Mul,a,b)

  fun (IL.I a) / (IL.I b) = I(Int.div(a,b))
    | (IL.D a) / (IL.D b) = D(Real./(a,b))
    | a / (IL.I 1) = a
    | a / b = Binop(Divv,a,b)

  infix %
  fun (IL.I a) % (IL.I b) = I(Int.mod(a,b))
    | (x as Binop(Modv,a,IL.I b)) % (IL.I c) = 
      if Int.<= (b,c) then x else Binop(Modv,x,IL.I c)
    | a % (IL.I 1) = I 0
    | a % b = Binop(Modv,a,b)

  fun min (IL.I a) (IL.I b) = I(if a < b then a else b)
    | min (IL.D a) (IL.D b) = D(if a < b then a else b)
    | min (y as IL.I d) (x as IL.If(a,IL.I b,IL.I c)) = 
      if Int.<=(b,d) andalso Int.<=(c,d) then x else
      if Int.<=(d,b) andalso Int.<=(d,c) then y else Binop(Min,y,x)
    | min a b = if eq(a,b) then a else Binop(Min,a,b)

  fun max (IL.I a) (IL.I b) = I(if a > b then a else b)
    | max (IL.D a) (IL.D b) = D(if a > b then a else b)
    | max (x as IL.If(a,IL.I b,IL.I c)) (y as IL.I d) = 
      if Int.>=(b,d) andalso Int.>=(c,d) then x else
      if Int.>=(d,b) andalso Int.>=(d,c) then y else Binop(Max,x,y)
    | max a b = if eq(a,b) then a else Binop(Max,a,b)

  fun (IL.I a) < (IL.I b) = B(Int.<(a,b))
    | (IL.D a) < (IL.D b) = B(Real.<(a,b))
    | (IL.Binop(Sub,IL.I a,b)) < (IL.I c) = I(Int.-(a,c)) < b
    | (IL.Binop(Sub,a,IL.I b)) < (IL.I c) = a < I(Int.+(b,c))
    | (IL.Binop(Add,IL.I a,b)) < (IL.I c) = b < I(Int.-(c,a))
    | (IL.Binop(Add,a,IL.I b)) < (IL.I c) = a < I(Int.-(c,b))
    | a < b = Binop(Lt,a,b)

  fun (IL.I a) <= (IL.I b) = B(Int.<=(a,b))
    | (IL.D a) <= (IL.D b) = B(Real.<=(a,b))
    | a <= b = Binop(Lteq,a,b)

  infix ==
  fun (IL.I a) == (IL.I b) = B(a=b)
    | (IL.D a) == (IL.D b) = B(Real.==(a,b))
    | IL.T == IL.T = IL.T
    | IL.F == IL.F = IL.T
    | IL.F == IL.T = IL.F
    | IL.T == IL.F = IL.F
    | (Binop(Mul,IL.I 2,_)) == (IL.I 1) = IL.F
    | (IL.Binop(Add,IL.I a,b)) == (IL.I c) = b == I(Int.-(c,a))
    | (IL.Binop(Add,a,IL.I b)) == (IL.I c) = a == I(Int.-(c,b))
    | a == b = 
      if eq(a,b) then IL.T 
      else
        case a of
          IL.If(e,x,y) =>
          (if nevereq(b,x) andalso nevereq(b,y) then IL.F
           else if eq(x,b) andalso nevereq(x,y) then e
           else case b of
                  IL.If(e',x',y') => if eq(x,x') andalso eq(y,y') andalso nevereq(x,y) then e == e'
                                     else Binop(Eq,a,b)
                | _ => Binop(Eq,a,b))
        | _ => case b of 
                 IL.If _ => b == a
               | _ => Binop(Eq,a,b)

  fun ~ e =
      case e of
        IL.I c => I (Int.~c)
      | IL.D c => D (Real.~c)
      | _ => Unop(Neg,e)
  fun i2d e =
      case e of
        IL.I c => D (real c)
      | _ => Unop(I2D,e)
  fun d2i e = Unop(D2I,e)
end
fun Subs(n,e) = IL.Subs(n,e)
val Alloc = IL.Alloc

val T = IL.T
val F = IL.F

type s = IL.Stmt
type ss = s list
val emp = IL.Nop
val Decl = IL.Decl
val Ret = IL.Ret
val Free = IL.Free
fun isEmp ss = List.all (fn IL.Nop => true | _ => false) ss
fun size ss =
    case ss of
      nil => 0
    | IL.Nop :: ss => size ss
    | s :: ss => Int.+(1, size ss)

fun unDecl (IL.Decl x) = SOME x
  | unDecl _ = NONE

val inlinethreshold = 3

fun For(e,f) ss =
    case e of
      IL.I 0 => ss
    | IL.I 1 => f (IL.I 0) @ ss
    | _ => 
      let val body = f($(Name.new IL.Int))
          fun default() = IL.For (e,f) :: ss
      in if isEmp body then ss
         else
           case e of
             IL.I n =>
             if Int.<(Int.*(size body, n), inlinethreshold) then
               let fun iter x f a =
                       if Int.<(x,0) then a
                       else iter (Int.-(x,1)) f (f(x,a))
               in iter (Int.-(n,1)) (fn (i,a) => f(I i) @ a) ss
               end
             else default()
           | _ => default()
      end

local open IL infix := ::= 
in
  fun n := e = 
     if IL.eq(e, $ n) then emp 
     else Assign(n,e)
  fun (n,i) ::= e = AssignArr(n, i, e)      
end

fun defs ss : N.set = 
    case ss of
      nil => N.empty
    | s::ss =>
      case s of
        IL.Nop => defs ss
      | IL.Ret e => N.empty
      | IL.Free n => defs ss
      | IL.Decl(n,e) => N.remove (defs ss,n)
      | IL.Assign(n,e) => N.insert (defs ss,n)
      | IL.AssignArr(n,e0,e) => defs ss
      | IL.For(e,f) =>
        let val n = Name.new Type.Int
            val ns = N.remove (defs(f($n)),n)
        in N.union (ns,defs ss)
        end

infix ::=
datatype info = EqI of e
              | LtI of e
              | GtEqI of e 
type env = (Name.t * info) list
val env_empty : env = nil
fun dom E = N.fromList(map #1 E)
fun env_cut E names =
    List.filter (fn (n,_) => not(N.member (names,n))) E

fun env_lookeq E n =
    case E of nil => NONE
            | (n', EqI e)::E => if n = n' then SOME e
                                else env_lookeq E n
            | _ :: E => env_lookeq E n

fun lt E e1 e2 =
    let fun look E n i =
            case E of
              (n', LtI(IL.I i'))::E => if n=n' andalso i>=i' then IL.T
                                       else look E n i
            | (n', GtEqI(IL.I i'))::E => if n=n' andalso Int.<=(i,i') then IL.F
                                         else look E n i
            | x::E => look E n i
            | nil => e1 < e2
    in
      case (e1,e2) of
        (IL.Var n, IL.I i) => look E n i
      | _ => e1 < e2
    end

fun modu E e1 e2 =
    let fun default() = e1 % e2
        fun look E n i =
            case E of
              (n', LtI(IL.I i'))::E => if n=n' andalso i>=i' then IL.Var n
                                       else look E n i
            | x::E => look E n i
            | nil => default()
    in
      case (e1,e2) of
        (IL.Var n, IL.I i) => look E n i
      | _ => default()
    end

fun se_e (E:env) (e:e) : e =
    case e of
      IL.Var n => (case env_lookeq E n of SOME e => se_e E e | NONE => $ n)
    | IL.I i => I i
    | IL.D d => D d
    | IL.T => B true
    | IL.F => B false
    | IL.If(e0,e1,e2) => If(se_e E e0, se_e E e1, se_e E e2)
    | IL.Subs(n,e) => Subs(n,se_e E e)
    | IL.Alloc(t,e) => Alloc(t,se_e E e)
    | IL.Binop(IL.Add,e1,e2) => (se_e E e1) + (se_e E e2)
    | IL.Binop(IL.Sub,e1,e2) => (se_e E e1) - (se_e E e2)
    | IL.Binop(IL.Mul,e1,e2) => (se_e E e1) * (se_e E e2)
    | IL.Binop(IL.Divv,e1,e2) => (se_e E e1) / (se_e E e2)
    | IL.Binop(IL.Modv,e1,e2) => modu E (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Min,e1,e2) => min (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Max,e1,e2) => max (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Lt,e1,e2) => lt E (se_e E e1) (se_e E e2)
    | IL.Binop(IL.Lteq,e1,e2) => (se_e E e1) <= (se_e E e2)
    | IL.Binop(IL.Eq,e1,e2) => (se_e E e1) == (se_e E e2)
    | IL.Unop(IL.Neg,e1) => ~(se_e E e1)
    | IL.Unop(IL.I2D,e1) => i2d (se_e E e1)
    | IL.Unop(IL.D2I,e1) => d2i (se_e E e1)

fun se_ss (E:env) (ss:ss) : ss * env =
    case ss of
      nil => (nil, env_empty)
    | s::ss2 =>
      case s of
        IL.Nop => se_ss E ss2
      | IL.Ret e => (Ret(se_e E e)::nil, env_empty)  (* ss2 is dead *)
      | IL.Free n => 
        let val (ss2,E2) = se_ss E ss2
        in (Free n :: ss2, E2)
        end
      | IL.Decl(n,e) =>
        let val e = se_e E e
            val E2 = (n,EqI e)::E
            val (ss2,E3) = se_ss E2 ss2
            val E3' = env_cut E3 (N.singleton n)
        in (Decl(n,e) :: ss2, E3')
        end
      | IL.Assign(n,e) =>
        let val e = se_e E e
            val E2 = (n,EqI e)::E
            val (ss2,E3) = se_ss E2 ss2
        in ((n := e) :: ss2, E3)
        end
      | IL.AssignArr(n,e0,e) =>
        let val e0 = se_e E e0
            val e = se_e E e                    
            val (ss2,E3) = se_ss E ss2
        in (((n,e0) ::= e) :: ss2, E3)
        end
      | IL.For(e,f) =>
        let val n = Name.new Type.Int
            val body = f($n)
            val defs_body = defs body
            val E = env_cut E defs_body
            val e' = se_e E e
            val E2 = (n,GtEqI(I 0))::(n,LtI e')::E
            val (ss1,E') = se_ss E2 body
            val (ss2',E2') = se_ss (env_cut E (dom E')) ss2
        in (For(e',fn e => #1(se_ss [(n,EqI e)] ss1)) ss2',
            E2')
        end
end
