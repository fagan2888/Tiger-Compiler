signature FINDESCAPE =
sig
  val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE =
struct

structure A = Absyn
structure S = Symbol

  type depth = int
  type escEnv = (depth * bool ref) S.table

  fun traverseVar (env : escEnv, d : depth, s : Absyn.var) : unit =
		let
				fun trvar (A.SimpleVar(id,pos)) = (case S.look(env,id) of SOME(id_depth,escape) => if id_depth>d then escape:=true else () | NONE => ())
					| trvar (A.FieldVar(v,id,pos)) = trvar v
					| trvar (A.SubscriptVar(v,exp,pos)) = (trvar v; traverseExp(env,d,exp))
		in
				trvar(s)
		end

	and traverseExp (env : escEnv, d : depth, s : Absyn.exp) : unit =
			let
					fun trexp (A.VarExp(var)) = traverseVar(env,d,var)
						| trexp (A.CallExp{func,args,pos}) = app trexp args
						| trexp (A.OpExp{left,oper,right,pos}) = (trexp left; trexp right)
						| trexp (A.RecordExp{fields,typ,pos}) = app trexp (map #2 fields)
						| trexp (A.SeqExp(expseq)) = app trexp (map #1 expseq)
						| trexp (A.AssignExp{var,exp,pos}) = (traverseVar(env,d,var); trexp exp)
						| trexp (A.IfExp{test,then',else',pos}) = (trexp test; trexp then'; case else' of SOME(expres) => trexp expres | _ => ())
						| trexp (A.WhileExp{test,body,pos}) = (trexp test; trexp body)
						| trexp (A.ForExp{var,escape,lo,hi,body,pos}) =
							let
									val new_env = S.enter(env,var,(d,escape))
							in
									traverseExp(new_env,d,lo);
									traverseExp(new_env,d,hi);
									traverseExp(new_env,d,body)
							end
						| trexp (A.LetExp{decs,body,pos}) =
							let
									val new_env = traverseDecs(env,d,decs)
							in
									traverseExp(new_env,d,body)
							end
						| trexp (A.ArrayExp{typ,size,init,pos}) = (trexp size; trexp init)
						| trexp _ = ()
			in
					trexp(s)
			end
										
  and traverseDecs (env, d, s : Absyn.dec list) : escEnv =
		let
				fun trdec (A.TypeDec(tydecs)) = env
					| trdec (A.VarDec{name,escape,typ,init,pos}) =
						let
								val new_env = S.enter(env,name,(d,escape))
						in
								traverseExp(new_env,d,init);
								new_env
						end
					| trdec (A.FunctionDec(fundecs)) =
						let
								fun addParam ({name,escape,typ,pos},env) =
									S.enter(env,name,(d+1,escape))
								fun trfundec ({name,params,result,body,pos}) =
									let
											val new_env = foldl addParam env params
									in
											traverseExp(new_env,d+1,body)
									end
						in
								app trfundec fundecs;
								env
						end
				and foldDec (dec, env) = trdec dec
		in
				foldl foldDec env s
		end

  fun findEscape (prog : Absyn.exp) : unit = traverseExp(S.empty,0,prog)

end
