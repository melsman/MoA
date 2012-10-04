signature ILUTIL = sig
  type Env
  val emptyEnv    : Env
  val add         : Env -> Name.t * IL.Value -> Env
  val lookup      : Env -> Name.t -> IL.Value option
  val eval        : Env -> Exp.e -> IL.Value
  val evalProgram : Env -> Program.p -> Env
  val ppProgram   : Program.p -> string
  val ppExp       : Exp.e -> string
  val ppValue     : IL.Value -> string
end
