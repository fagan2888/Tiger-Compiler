signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env : ENV =
struct
  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}

  fun insert ((name, info), table) = Symbol.enter(table, Symbol.symbol(name), info)
  val base_tenv = foldl insert Symbol.empty [("int",Types.INT),("string",Types.STRING)]
  val base_venv =
    let
      val functions = [("print", FunEntry({formals=[Types.STRING],result=Types.UNIT})),
                       ("flush",FunEntry({formals=[],result=Types.UNIT})),
                       ("getchar",FunEntry({formals=[],result=Types.STRING})),
                       ("ord",FunEntry({formals=[Types.STRING],result=Types.INT})),
                       ("chr",FunEntry({formals=[Types.INT],result=Types.STRING})),
                       ("size",FunEntry({formals=[Types.STRING],result=Types.INT})),
                       ("substring",FunEntry({formals=[Types.STRING,Types.INT,Types.INT],result=Types.STRING})),
                       ("concat",FunEntry({formals=[Types.STRING,Types.STRING],result=Types.STRING})),
                       ("not",FunEntry({formals=[Types.INT],result=Types.INT})),
                       ("exit",FunEntry({formals=[Types.INT],result=Types.UNIT}))]
    in
      foldl insert Symbol.empty functions
    end
end
