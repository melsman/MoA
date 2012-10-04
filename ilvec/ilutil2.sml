structure ILUtil :> ILUTIL = struct
  open IL

  fun die s = raise Fail ("ILUtil." ^ s)

  fun iter f a (i,j) =
      let fun loop a n = if n > j then a
                         else loop (f (n,a)) (n+1)
      in loop a i
      end
      
  type Env = (Name * Value) list
  val empty = []
  fun add e (n,v) = (Name.pr n,v)::e
  fun lookup E n =
      let val n = Name.pr n
      in case List.find (fn (x,_) => x=n) E of
           SOME(_,v) => SOME v
         | NONE => NONE
      end

  fun evalBinOp Add (IntV i1,IntV i2) = IntV(i1+i2)
    | evalBinOp Sub (IntV i1,IntV i2) = IntV(i1-i2)
    | evalBinOp Mul (IntV i1,IntV i2) = IntV(i1*i2)
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
        Var n => (case lookup E (Name.fromString n) of
                    SOME v => v
                  | NONE => die("lookup: " ^ n))
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
           IntV i => (case lookup E (Name.fromString n) of
                        SOME(ArrV v) => 
                        (case ! (Vector.sub(v,i)) of
                           SOME v => v
                         | NONE => die "eval.Subs.array value not initialized")                       
                      | _ => die("eval.Subs.lookup: " ^ n))
         | _ => die "eval.Subs.expecting integer")
      | Alloc e1 =>
        (case eval E e1 of
           IntV n => ArrV(Vector.tabulate(n,fn _ => ref NONE))
         | _ => die "eval.Alloc.expecting integer")
      | If(e0,e1,e2) =>
        (case eval E e0 of
           BoolV b => eval E (if b then e1 else e2)
         | _  => die "eval.If.expecting boolean")

  fun evalProgram E (p: Program) : Env =
      case p of
        For (e, f) =>
        (case eval E e of
           IntV n =>
           let val name = Name.new ()
               val body = f (Name.pr name)
           in iter (fn (i,E) => 
                       let val E = add E (name,IntV i)
                       in evalProgram E body
                       end) E (0,n-1)
           end
         | _ => die "For")
      | Assign (n,e) => add E (Name.fromString n, eval E e)
      | AssignArr (n,i,e) =>
        (case eval E i of
           IntV i =>
           let val v = eval E e
           in case lookup E (Name.fromString n) of
                SOME(ArrV vec) =>
                let val r = Vector.sub(vec,i)
                in r := SOME v; E
                end
              | _ => die "eval.AssignArr.couldn't find vector in env"
           end
         | _ => die "eval.AssignArr.expecting int as index")
      | Seq ps => List.foldl (fn (p,E) => evalProgram E p) E ps
      | Free n => die "Free.unimplemented"

  val eval = fn e => fn te => eval e (Exp.toExp te)
  val evalProgram = fn e => fn p => evalProgram e (Program.toProgram p)

  val emptyEnv = []

  datatype rope = % of string
                | %% of rope * rope
  infix %%
  fun ropeToString r =
      let fun loop a = fn
              % s => s :: a
            | r1 %% r2 => loop (loop a r1) r2
      in (String.concat o rev o loop nil) r
      end

  fun par e = %"(" %% e %% %")"
  fun spar e = %"[" %% e %% %"]"

  fun ppB Add = "+"
    | ppB Sub = "-"
    | ppB Mul = "*"
    | ppB Min = "min"
    | ppB Max = "max"
    | ppB Lt = "<"
    | ppB Lteq = "<="
    | ppB Eq = "=="

  fun infi x = List.exists (fn y => x = y) [Add,Sub,Mul,Lt,Lteq,Eq]

  fun ppU Neg = "-"

  fun pp e =
      case e of
        Var n => %n
      | Int i => % (Int.toString i)
      | Binop(binop,e1,e2) => 
        if infi binop then par (pp e1 %% % (ppB binop) %% pp e2)
        else % (ppB binop) %% par(pp e1 %% %"," %% pp e2)
      | Unop(unop,e1) => %(ppU unop) %% (pp e1)
      | App (e1,e2) => pp e1 %% par(pp e2)
      | Alloc e1 => %"alloc" %% par(pp e1)
      | Subs(n,e1) => %n %% spar(pp e1)
      | T => %(Bool.toString true)
      | F => %(Bool.toString false)
      | If(e0,e1,e2) => pp e0 %%  %" ? " %% pp e1 %% %" : " %% pp e2

  fun ppP p =
      case p of
        For (e, f) =>
        let val n = Name.pr(Name.new ())
        in % ("for (int " ^ n ^ " = 0; " ^ n ^ " < ") %%
             pp e %% %("; " ^ n ^ "++) {\n") %%
             ppP(f n) %%
             %"}\n"
        end
      | Assign (n,e) => %n %% %" = " %% pp e %% %";\n"
      | AssignArr (n,i,e) => %n %% spar(pp i) %% %" = " %% pp e %% %";\n"
      | Seq ps => List.foldl (fn (p,r) => r %% ppP p) (%"") ps
      | Free n => die "Free.unimplemented"

  fun ppProgram p = ropeToString(ppP (Program.toProgram p))
  fun ppExp e = ropeToString(pp (Exp.toExp e))

  fun ppValue v = 
      case v of
      IntV i => Int.toString i
    | BoolV b => Bool.toString b
    | FunV _ => "fn"
    | ArrV v => "vec"
end
