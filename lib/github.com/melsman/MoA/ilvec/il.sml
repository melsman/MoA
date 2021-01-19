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
       | Ifp of Exp * Program * Program
       | Assign of Name * Exp
       | AssignArr of Name * Exp * Exp
       | Seq of Program list
       | Free of Name
end
                               
signature TYPE = sig
  eqtype 'a t
  eqtype 'a arr
  val VAR : unit -> 'a t
  val INT : int t
  val BOOL : bool t
  val ARRAY : 'a t -> 'a arr t
  val pr  : 'a t -> string
end

structure Type :> TYPE = struct
  type 'a t = string
  type 'a arr = unit
  fun VAR() = "'a"
  val INT = "int"
  val BOOL = "bool"
  val ARRAY = fn t => t ^ " arr"
  fun pr s = s
end

signature NAME = sig
  eqtype 'a t
  val new : 'a Type.t -> 'a t
  val pr  : 'a t -> string
  val fromString : string -> 'a t
end

structure Name :> NAME = struct
  type 'a t = string
  val count = ref 0
  fun new t = "n" ^ Int.toString(!count) before count := !count + 1
  fun pr s = s
  fun fromString s = s
end

signature EXP = sig
  type 'a e
  val V     : 'a Name.t -> 'a e
  val Apply : ('a -> 'b) e -> 'a e -> 'b e
  val Subs  : 'a Type.arr Name.t * int e -> 'a e
  val Alloc : int e -> 'a Type.arr e
  val I     : int -> int e
  val T     : bool e
  val F     : bool e
  val If    : bool e * 'a e * 'a e -> 'a e
  val +     : int e * int e -> int e
  val -     : int e * int e -> int e
  val *     : int e * int e -> int e
  val <     : int e * int e -> bool e
  val <=    : int e * int e -> bool e
  val ==    : int e * int e -> bool e
  val max   : int e -> int e -> int e
  val min   : int e -> int e -> int e
  val toExp : 'a e -> IL.Exp
end

signature PROGRAM = sig
  type p
  type 'a e
  val For : int e * (int Name.t -> p) -> p
  val Ifp : bool e * p * p -> p
  val :=  : 'a Name.t * 'a e -> p
  val ::= : ('a Type.arr Name.t * int e) * 'a e -> p
  val >>  : p * p -> p
  val emp : p
  val toProgram : p -> IL.Program
end

structure Exp :> EXP = struct
local open IL
in
  type 'a e = Exp
  fun toExp e = e
  fun V n = Var (Name.pr n)
  fun Apply _ _ = raise Fail "Apply not implemented"
  fun I n = Int n

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

structure Program :> PROGRAM where type 'a e = 'a Exp.e = struct
  type p = IL.Program
  type 'a e = 'a Exp.e
  val For = fn (e,f) => IL.For (Exp.toExp e,fn n => f(Name.fromString n))
  val Ifp = fn (e,p1,p2) => IL.Ifp (Exp.toExp e,p1,p2)
  local open IL infix := ::= >>
  in fun n := e = Assign(Name.pr n,Exp.toExp e)
     fun (n,i) ::= e = AssignArr(Name.pr n,Exp.toExp i,Exp.toExp e)
     fun a >> b = Seq[a,b]
     val emp = Seq[]
     fun toProgram p = p
  end
end
