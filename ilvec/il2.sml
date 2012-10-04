structure IL = struct
type Name = string

datatype Value =
         IntV of int
       | BoolV of bool
       | FunV of Value -> Value
       | ArrV of Value option ref vector
datatype Unop = Neg
datatype Binop = Add | Sub | Mul | Min | Max | Lt | Lteq | Eq
datatype Exp =
         Var of Name
       | Int of int
       | T | F
       | If of Exp * Exp * Exp
       | Subs of Name * Exp
       | Alloc of Exp
       | Binop of Binop * Exp * Exp
       | Unop of Unop * Exp
       | App of Exp * Exp
                
type Size = Exp
type Index = Exp
             
datatype Program =
         For of Exp * (Name -> Program)
       | Assign of Name * Exp
       | AssignArr of Name * Exp * Exp
       | Seq of Program list
       | Free of Name
end

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

signature EXP = sig
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
  val toExp : e -> IL.Exp
end

signature PROGRAM = sig
  type p
  type e
  val For : e * (Name.t -> p) -> p
  val :=  : Name.t * e -> p
  val ::= : (Name.t * e) * e -> p
  val >>  : p * p -> p
  val emp : p
  val toProgram : p -> IL.Program
end

structure Exp :> EXP = struct
local open IL
in
  type e = Exp
  fun toExp e = e
  fun $ n = Var (Name.pr n)
  fun I n = Int n
  fun B true = T
    | B false = F 
  fun (Int a) - (Int b) = I(Int.-(a,b))
    | a       - b       = Binop(Sub,a,b)

  fun (Int 0)              + b       = b
    | a                    + (Int 0) = a
    | (Int a)              + (Int b) = I(Int.+(a,b))
    | (Binop(Sub,a,Int b)) + (Int c) = let val d = Int.-(c,b)
                                       in if d > 0 then a + (I d)
                                          else a - I (~d)
                                       end
    | a                    + b       = Binop(Add,a,b)

  fun a * b = Binop(Mul,a,b)
  fun min (Int a) (Int b) = I(if a < b then a else b)
    | min a b = Binop(Min,a,b)
  fun max (Int a) (Int b) = I(if a > b then a else b)
    | max a b = Binop(Max,a,b)

  fun a < b = Binop(Lt,a,b)
  fun a <= b = Binop(Lteq,a,b)
  infix ==
  fun a == b = Binop(Eq,a,b)
end
fun Subs(n,e) = IL.Subs(Name.pr n, e)
val Alloc = IL.Alloc
val If = IL.If
val T = IL.T
val F = IL.F
end

structure Program :> PROGRAM where type e = Exp.e = struct
  type p = IL.Program
  type e = Exp.e
  val For = fn (e,f) => IL.For (Exp.toExp e,fn n => f(Name.fromString n))
  local open IL infix := ::= >>
  in fun n := e = Assign(Name.pr n,Exp.toExp e)
     fun (n,i) ::= e = AssignArr(Name.pr n,Exp.toExp i,Exp.toExp e)
     fun a >> b = Seq[a,b]
     val emp = Seq[]
     fun toProgram p = p
  end
end
