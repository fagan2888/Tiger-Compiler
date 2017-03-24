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
      val functions = [("print", FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_print"),formals=[Types.STRING],result=Types.UNIT})),
                       ("flush",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_flush"),formals=[],result=Types.UNIT})),
                       ("getchar",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_getchar"),formals=[],result=Types.STRING})),
                       ("ord",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_ord"),formals=[Types.STRING],result=Types.INT})),
                       ("chr",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_chr"),formals=[Types.INT],result=Types.STRING})),
                       ("size",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_size"),formals=[Types.STRING],result=Types.INT})),
                       ("substring",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_substring"),formals=[Types.STRING,Types.INT,Types.INT],result=Types.STRING})),
                       ("concat",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_concat"),formals=[Types.STRING,Types.STRING],result=Types.STRING})),
                       ("not",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_not"),formals=[Types.INT],result=Types.INT})),
                       ("exit",FunEntry({level=Translate.outermost,label=Temp.namedlabel("tiger_exit"),formals=[Types.INT],result=Types.UNIT}))]
    in
      foldl insert Symbol.empty functions
    end
end
