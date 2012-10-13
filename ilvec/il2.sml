signature NAME = sig
  eqtype t
  val new : unit -> t
  val pr  : t -> string
  val fromString : string -> t
end

structure Name :> NAME = struct
  type t = string
  val count = ref 0
  fun new () = "n" ^ Int.toString(!count) before count := !count + 1
  fun pr s = s
  fun fromString s = s
end
 
structure IL = struct
datatype Value =
         IntV of int
       | BoolV of bool
       | FunV of Value -> Value
       | ArrV of Value option ref vector
datatype Unop = Neg
datatype Binop = Add | Sub | Mul | Min | Max | Lt | Lteq | Eq
datatype Exp =
         Var of Name.t
       | Int of int
       | T | F
       | If of Exp * Exp * Exp
       | Subs of Name.t * Exp
       | Alloc of Exp
       | Binop of Binop * Exp * Exp
       | Unop of Unop * Exp
       | App of Exp * Exp
                
type Size = Exp
type Index = Exp
             
datatype Program =
         For of Exp * (Exp -> Program)
       | Assign of Name.t * Exp
       | AssignArr of Name.t * Exp * Exp
       | Seq of Program list
       | Free of Name.t
end

signature PROGRAM = sig
  type e
  val $     : Name.t -> e
  val Subs  : Name.t * e -> e
  val Alloc : e -> e
  val I     : int -> e
  val B     : bool -> e
  val If    : e * e * e -> e
  val +     : e * e -> e
  val -     : e * e -> e
  val *     : e * e -> e
  val <     : e * e -> e
  val <=    : e * e -> e
  val ==    : e * e -> e
  val max   : e -> e -> e
  val min   : e -> e -> e
  val unI   : e -> int option

  type p
  val For : e * (e -> p) -> p
  val :=  : Name.t * e -> p
  val ::= : (Name.t * e) * e -> p
  val >>  : p * p -> p
  val emp : p
end

structure Program : PROGRAM = struct
local open IL
in
  type e = Exp
  fun toExp e = e
  fun $ n = Var n
  fun I n = Int n
  fun unI (Int n) = SOME n
    | unI _ = NONE
  fun B true = T
    | B false = F 
  fun a       - (Int 0) = a
    | (Int a) - (Int b) = I(Int.-(a,b))
    | a       - b       = Binop(Sub,a,b)

  fun (Int 0)              + b       = b
    | a                    + (Int 0) = a
    | (Int a)              + (Int b) = I(Int.+(a,b))
    | (Binop(Sub,a,Int b)) + (Int c) = let val d = Int.-(c,b)
                                       in if d > 0 then a + (I d)
                                          else a - I (~d)
                                       end
    | a                    + b       = Binop(Add,a,b)

  fun (Int a) * (Int b) = I(Int.*(a,b))
    | (Int 1) * b       = b
    | a       * (Int 1) = a
    | (Int 0) * b       = I 0
    | a       * (Int 0) = I 0
    | a       * b       = Binop(Mul,a,b)

  fun min (Int a) (Int b) = I(if a < b then a else b)
    | min a b = Binop(Min,a,b)

  fun max (Int a) (Int b) = I(if a > b then a else b)
    | max a b = Binop(Max,a,b)

  fun (Int a) < (Int b) = B(Int.<(a,b))
    | a < b = Binop(Lt,a,b)
  fun (Int a) <= (Int b) = B(Int.<=(a,b))
    | a <= b = Binop(Lteq,a,b)

  infix ==
  fun (Int a) == (Int b) = B(a=b)
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

type p = IL.Program
val emp = IL.Seq[]
fun isEmp (IL.Seq[]) = true
  | isEmp _ = false

fun For(e,f) =
    case e of
      IL.Int 0 => emp
    | IL.Int 1 => f (IL.Int 0)
    | _ => if isEmp(f($(Name.new()))) then emp
           else IL.For (e,f)

local open IL infix := ::= >>
in
   fun n := e = 
       if e = $ n then emp 
       else Assign(n,e)
   fun (n,i) ::= e = AssignArr(n, i, e)
   fun (Seq[]) >> b = b
     | a >> (Seq[]) = a
     | a >> b = Seq[a,b]
   fun toProgram p = p
end
end
