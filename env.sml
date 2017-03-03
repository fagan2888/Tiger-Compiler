structure S = Symbol
structure T = Types

signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty S.table
  val base_venv : enventry S.table
end

structure Env : ENV =
struct
  type access = unit
  type ty = T.ty
  datatype enventry = VarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result: T.ty}

  fun insert ((name, info), table) = S.enter(table, S.symbol(name), info)
  val base_tenv = foldl insert S.empty [("int",T.INT),("string",T.STRING)]
  val base_venv =
    let
      val functions = [("print", FunEntry({formals=[T.STRING],result=T.UNIT})),
                       ("flush",FunEntry({formals=[],result=T.UNIT})),
                       ("getchar",FunEntry({formals=[],result=T.STRING})),
                       ("ord",FunEntry({formals=[T.STRING],result=T.INT})),
                       ("chr",FunEntry({formals=[T.INT],result=T.STRING})),
                       ("size",FunEntry({formals=[T.STRING],result=T.INT})),
                       ("substring",FunEntry({formals=[T.STRING,T.INT,T.INT],result=T.STRING})),
                       ("concat",FunEntry({formals=[T.STRING,T.STRING],result=T.STRING})),
                       ("not",FunEntry({formals=[T.INT],result=T.INT})),
                       ("exit",FunEntry({formals=[T.INT],result=T.UNIT}))]
    in
      foldl insert S.empty functions
    end
end
