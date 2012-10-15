structure ILUtil : ILUTIL = struct
  open IL

  type e = Program.e
  type s = Program.s
  type ss = Program.s list
  type Value = IL.Value

  fun die s = raise Fail ("ILUtil." ^ s)

  fun iter f a (i,j) =
      let fun loop a n = if n > j then a
                         else loop (f (n,a)) (n+1)
      in loop a i
      end
      
  type Env = (Name.t * Value) list
  val empty = []
  fun add e (n,v) = (n,v)::e
  fun lookup E n =
      case List.find (fn (x,_) => x=n) E of
        SOME(_,v) => SOME v
      | NONE => NONE

  fun evalBinOp Add (IntV i1,IntV i2) = IntV(i1+i2)
    | evalBinOp Sub (IntV i1,IntV i2) = IntV(i1-i2)
    | evalBinOp Mul (IntV i1,IntV i2) = IntV(i1*i2)
    | evalBinOp Divv (IntV i1,IntV i2) = IntV(i1 div i2)
    | evalBinOp Min (IntV i1,IntV i2) = IntV(if i1 < i2 then i1 else i2)
    | evalBinOp Max (IntV i1,IntV i2) = IntV(if i1 > i2 then i1 else i2)
    | evalBinOp Lt  (IntV i1,IntV i2) = BoolV(i1 < i2)
    | evalBinOp Lteq  (IntV i1,IntV i2) = BoolV(i1 <= i2)
    | evalBinOp Eq  (IntV i1,IntV i2) = BoolV(i1 = i2)
    | evalBinOp _ _ = die "evalBinOp"
        
  fun evalUnOp Neg (IntV i1) = IntV(~i1)
    | evalUnOp _ _ = die "evalUnOp"

  fun eval (E:Env) (e:Exp) : Value =
      case e of
        Var n => (case lookup E n of
                    SOME v => v
                  | NONE => die("lookup: " ^ Name.pr n))
      | Int i => IntV i
      | T => BoolV true
      | F => BoolV false
      | Binop(binop,e1,e2) => evalBinOp binop (eval E e1, eval E e2)
      | Unop(unop,e1) => evalUnOp unop (eval E e1)
      | App (e1,e2) =>
        (case eval E e1 of
           FunV f => f (eval E e2)
         | _ => die "eval.expecting function for function application")        
      | Subs(n,e1) =>
        (case eval E e1 of
           IntV i => (case lookup E n of
                        SOME(ArrV v) => 
                        (case ! (Vector.sub(v,i)) of
                           SOME v => v
                         | NONE => die "eval.Subs.array value not initialized")                       
                      | _ => die("eval.Subs.lookup: " ^ Name.pr n))
         | _ => die "eval.Subs.expecting integer")
      | Alloc e1 =>
        (case eval E e1 of
           IntV n => ArrV(Vector.tabulate(n,fn _ => ref NONE))
         | _ => die "eval.Alloc.expecting integer")
      | If(e0,e1,e2) =>
        (case eval E e0 of
           BoolV b => eval E (if b then e1 else e2)
         | _  => die "eval.If.expecting boolean")

  fun evalS E (s: Stmt) : Env =
      case s of
        For (e, f) =>
        (case eval E e of
           IntV n =>
           let val name = Name.new ()
               val ss = f (Var name)
           in iter (fn (i,E) => 
                       let val E = add E (name,IntV i)
                       in evalSS E ss
                       end) E (0,n-1)
           end
         | _ => die "For")
      | Ret e => add E (Name.result, eval E e)
      | Assign (n,e) => add E (n, eval E e)
      | AssignArr (n,i,e) =>
        (case eval E i of
           IntV i =>
           let val v = eval E e
           in case lookup E n of
                SOME(ArrV vec) =>
                let val r = Vector.sub(vec,i)
                in r := SOME v; E
                end
              | _ => die "eval.AssignArr.couldn't find vector in env"
           end
         | _ => die "eval.AssignArr.expecting int as index")
      | Free n => die "Free.unimplemented"
      | Nop => E

  and evalSS E ss =
      List.foldl (fn (s,E) => evalS E s) E ss

  val emptyEnv = []

  datatype rope = % of string
                | %% of rope * rope
                | %> of rope
                | %$
  fun repeat s 0 = ""
    | repeat s n = s ^ repeat s (n-1)
  infix %%
  fun ropeToString n r =
      let fun loop n a = fn
              % s => s :: a
            | %$ => ("\n" ^ repeat "  " n) :: a
            | %> r => loop (n+1) a r
            | r1 %% r2 => loop n (loop n a r1) r2
      in (String.concat o rev o (loop n nil)) r
      end

  fun par e = %"(" %% e %% %")"
  fun spar e = %"[" %% e %% %"]"
  fun cpar e = %"{" %% e %% %"}"

  fun ppB Add = "+"
    | ppB Sub = "-"
    | ppB Mul = "*"
    | ppB Divv = "/"
    | ppB Min = "min"
    | ppB Max = "max"
    | ppB Lt = "<"
    | ppB Lteq = "<="
    | ppB Eq = "=="

  fun infi x = List.exists (fn y => x = y) [Add,Sub,Mul,Divv,Lt,Lteq,Eq]

  fun ppU Neg = "-"

  fun pp e =
      case e of
        Var n => %(Name.pr n)
      | Int i => %(Int.toString i)
      | Binop(binop,e1,e2) => 
        if infi binop then par (pp e1 %% % (ppB binop) %% pp e2)
        else % (ppB binop) %% par(pp e1 %% %"," %% pp e2)
      | Unop(unop,e1) => %(ppU unop) %% (pp e1)
      | App (e1,e2) => pp e1 %% par(pp e2)
      | Alloc e1 => %"alloc" %% par(pp e1)
      | Subs(n,e1) => %(Name.pr n) %% spar(pp e1)
      | T => %(Bool.toString true)
      | F => %(Bool.toString false)
      | If(e0,e1,e2) => par(pp e0 %%  %" ? " %% pp e1 %% %" : " %% pp e2)

  fun ppSS0 ss =
      case ss of
        nil => %""
      | Nop :: ss => ppSS0 ss
      | s :: ss => %$ %% ppS s %% ppSS0 ss
 
  and ppS s =
      case s of
        For (e, f) =>
        let val n = Name.new()
            val ns = Name.pr n 
        in %("for (int " ^ ns ^ " = 0; " ^ ns ^ " < ") %%
            pp e %% %("; " ^ ns ^ "++) {") %% 
              %>(ppSS0(f (Var n))) %%
            %$ %% %"}"
        end
      | Assign (n,e) => %(Name.pr n) %% %" = " %% pp e %% %";"
      | AssignArr (n,i,e) => %(Name.pr n) %% spar(pp i) %% %" = " %% pp e %% %";"
      | Nop => %""
      | Free n => die "Free.unimplemented"
      | Ret e => %"return " %% pp e %% %";"

  fun ppSS n ss = ropeToString n (%$ %% ppSS0 ss)
  fun ppExp e = ropeToString 0 (pp e)

  fun ppFunction name argname ss =
      let val r =
              %name %% par(%(Name.pr argname)) %% %" " %% cpar(
              %>(ppSS0 ss) %% %$) %% %$
      in ropeToString 0 r
      end

  fun ppValue v = 
      case v of
      IntV i => Int.toString i
    | BoolV b => Bool.toString b
    | FunV _ => "fn"
    | ArrV v => "vec"
end
