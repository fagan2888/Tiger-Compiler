signature FINDESCAPE =
sig
  val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE =
struct
  type depth = int
  type escEnv = {depth * bool ref} Symbol.table

  fun traverseVar (env : escEnv, d : depth, s : Absyn.var) : unit =

	and traverseExp (env : escEnv, d : depth, s : Absyn.var) : unit =
			let
					fun trexp (A.VarExp(var)) = traverseVar(env,d,var)
						| trexp (A.CallExp{func,args,pos}) = ()
						| trexp (A.OpExp(left,oper,right,pos) = (traverseExp(left); traverseExp(right))

					
		
  fun traverseDecs (env, d, s : Absyn.dec list) : escEnv =

  fun findEscape (prog : Absyn.exp) : unit =

end
