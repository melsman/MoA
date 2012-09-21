signature ILUTIL = sig
  type Env
  val emptyEnv    : Env
  val add         : Env -> 'a Name.t * IL.Value -> Env
  val lookup      : Env -> 'a Name.t -> IL.Value option
  val eval        : Env -> 'a Exp.e -> IL.Value
  val evalProgram : Env -> Program.p -> Env
  val ppProgram   : Program.p -> string
  val ppExp       : 'a Exp.e -> string
  val ppValue     : IL.Value -> string
end
