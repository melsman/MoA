structure IL = struct

datatype Type = Int | Bool | Vec of Type

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

datatype Value =
         IntV of int
       | BoolV of bool
       | ArrV of Value option ref vector
datatype Unop = Neg
datatype Binop = Add | Sub | Mul | Divv | Min | Max | Lt | Lteq | Eq
datatype Exp =
         Var of Name.t
       | I of int
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
end

signature TYPE = sig
  eqtype Int and Bool and 'a Vec     (* Types *)
  eqtype 'a T                        (* Type constructors *)
  val Int      : Int T
  val Bool     : Bool T
  val Vec      : 'a T -> 'a Vec T
  val prType   : 'a T -> string
  val vecElem  : 'a Vec T -> 'a T
end

structure Type : TYPE = struct
  type Int = unit
   and Bool = unit
   and 'a Vec = unit

  type 'a T = IL.Type
  val Int = IL.Int
  val Bool = IL.Bool
  val Vec = IL.Vec
  fun prType IL.Int = "int"
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
  val B     : bool -> e
  val If    : e * e * e -> e
  val +     : e * e -> e
  val -     : e * e -> e
  val *     : e * e -> e
  val /     : e * e -> e
  val <     : e * e -> e
  val <=    : e * e -> e
  val ==    : e * e -> e
  val max   : e -> e -> e
  val min   : e -> e -> e
  val unI   : e -> int option

  type s
  type ss = s list
  val For : e * (e -> ss) -> ss -> ss
  val :=  : Name.t * e -> s
  val ::= : (Name.t * e) * e -> s
  val Decl: Name.t * e -> s
  val Ret : e -> s
  val emp : s
end

structure Program : PROGRAM = struct
local open IL
in
  type e = Exp
  fun toExp e = e
  fun $ n = Var n
  fun I n = IL.I n
  fun unI (IL.I n) = SOME n
    | unI _ = NONE
  fun B true = T
    | B false = F 
  fun a        - (IL.I 0) = a
    | (IL.I a) - (IL.I b) = I(Int.-(a,b))
    | a        - b       = Binop(Sub,a,b)

  fun (IL.I 0)              + b       = b
    | a                     + (IL.I 0) = a
    | (IL.I a)              + (IL.I b) = I(Int.+(a,b))
    | (Binop(Sub,a,IL.I b)) + (IL.I c) = let val d = Int.-(c,b)
                                         in if d > 0 then a + (I d)
                                            else a - I (~d)
                                         end
    | a                    + b         = Binop(Add,a,b)

  fun (IL.I a) * (IL.I b) = I(Int.*(a,b))
    | (IL.I 1) * b        = b
    | a        * (IL.I 1) = a
    | (IL.I 0) * b        = I 0
    | a        * (IL.I 0) = I 0
    | a        * b        = Binop(Mul,a,b)

  fun (IL.I a) / (IL.I b) = I(Int.div(a,b))
    | a / (IL.I 1) = a
    | a / b = Binop(Divv,a,b)

  fun min (IL.I a) (IL.I b) = I(if a < b then a else b)
    | min a b = Binop(Min,a,b)

  fun max (IL.I a) (IL.I b) = I(if a > b then a else b)
    | max a b = Binop(Max,a,b)

  fun (IL.I a) < (IL.I b) = B(Int.<(a,b))
    | a < b = Binop(Lt,a,b)
  fun (IL.I a) <= (IL.I b) = B(Int.<=(a,b))
    | a <= b = Binop(Lteq,a,b)

  infix ==
  fun (IL.I a) == (IL.I b) = B(a=b)
    | IL.T == IL.T = IL.T
    | IL.F == IL.F = IL.T
    | IL.F == IL.T = IL.F
    | IL.T == IL.F = IL.F
    | a == b = Binop(Eq,a,b)
end
fun Subs(n,e) = IL.Subs(n,e)
val Alloc = IL.Alloc
fun If(IL.T,b,c) = b
  | If(IL.F,b,c) = c
  | If(a,b,c) = if b = c then b 
                else if b = IL.T andalso a = c then a
                else if c = IL.F andalso a = b then a
                else if b = IL.F andalso a = c then IL.F
                else if c = IL.T andalso a = b then IL.T
                else IL.If(a,b,c)
val T = IL.T
val F = IL.F

type s = IL.Stmt
type ss = s list
val emp = IL.Nop
val Decl = IL.Decl
val Ret = IL.Ret
fun isEmp ss = List.all (fn IL.Nop => true | _ => false) ss

fun For(e,f) ss =
    case e of
      IL.I 0 => ss
    | IL.I 1 => f (IL.I 0) @ ss
    | _ => if isEmp(f($(Name.new IL.Int))) then ss
           else IL.For (e,f) :: ss

local open IL infix := ::= 
in
  fun n := e = 
     if e = $ n then emp 
     else Assign(n,e)
  fun (n,i) ::= e = AssignArr(n, i, e)      
end
end
