signature FINDESCAPE =
sig
  val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE =
struct
  type depth = int
  type escEnv = {depth * bool ref} Symbol.table

  fun traverseVar (env : escEnv, d : depth, s : Absyn.var) : unit =
  fun traverseExp (env : escEnv, d : depth, s : Absyn.var) : unit =
  fun traverseDecs (env, d, s : Absyn.dec list) : escEnv =

  fun findEscape (prog : Absyn.exp) : unit =

end
