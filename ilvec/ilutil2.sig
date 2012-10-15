signature ILUTIL = sig
  type e
  type ss
  type Env
  type Value = IL.Value
  val emptyEnv    : Env
  val add         : Env -> Name.t * Value -> Env
  val lookup      : Env -> Name.t -> Value option
  val eval        : Env -> e -> Value
  val evalSS      : Env -> ss -> Env
  val ppSS        : int -> ss -> string  (* int is indent level *)
  val ppExp       : e -> string
  val ppFunction  : string -> Name.t -> ss -> string
  val ppValue     : Value -> string
end
