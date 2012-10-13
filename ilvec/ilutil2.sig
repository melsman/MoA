signature ILUTIL = sig
  type e
  type p
  type Env
  type Value = IL.Value
  val emptyEnv    : Env
  val add         : Env -> Name.t * Value -> Env
  val lookup      : Env -> Name.t -> Value option
  val eval        : Env -> e -> Value
  val evalProgram : Env -> p -> Env
  val ppProgram   : int -> p -> string  (* int is indent level *)
  val ppExp       : e -> string
  val ppFunction  : string -> Name.t -> p -> string
  val ppValue     : Value -> string
end
