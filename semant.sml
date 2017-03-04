structure A = Absyn
structure S = Symbol
structure T = Types
structure E = Env

signature SEMANT =
sig
  val transProg : A.exp -> unit
end

structure Semant : SEMANT =
struct
  structure Translate = struct type exp = unit end
  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = E.enventry S.table
  type tenv = T.ty S.table

  fun checkInt ({exp=exp,ty=T.INT},pos) = ()
	  | checkInt ({exp=exp,ty=_},pos) = ErrorMsg.error pos "integer argument expected"

  (*
     TransVar
     TransDec
     TransTy
  *)

  fun transExp (venv, tenv) =
    let
      fun (* VarExp
      | *)trexp (A.NilExp) = {exp=(), ty=T.NIL}
        | trexp (A.IntExp(int)) = {exp=(), ty=T.INT}
				| trexp (A.StringExp(string,pos)) = {exp=(), ty=T.STRING}
        (* CallExp *)
        | trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.LtOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.GtOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.LeOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
				| trexp (A.OpExp{left,oper=A.GeOp,right,pos}) = (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty=Types.INT})
        (*
           RecordExp
           SeqExp
           AssignExp
           IfExp
           WhileExp
           ForExp
           BreakExp
           LetExp
           ArrayExp
        *)
    in
      trexp
    end

  fun transProg exp =
    let
      val expty = (transExp (E.base_venv, E.base_tenv) exp)
    in
      ()
    end

end
