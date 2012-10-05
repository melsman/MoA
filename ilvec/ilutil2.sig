signature ILUTIL = sig
  type e
  type p
  type Env
  type Value
  val emptyEnv    : Env
  val add         : Env -> Name.t * Value -> Env
  val lookup      : Env -> Name.t -> Value option
  val eval        : Env -> e -> Value
  val evalProgram : Env -> p -> Env
  val ppProgram   : p -> string
  val ppExp       : e -> string
  val ppValue     : Value -> string
end
