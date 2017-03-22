structure T = Tree

signature TRANSLATE =
sig
  type level
  type access
  val outermost: level
  val newLevel: {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val frags: Frame.frag list ref
  val procEntryExit: {level: level, body: exp} -> unit
  val getResult: unit -> Frame.frag list

  val simpleVar: access * level -> exp (* TODO *)
  val subscriptVar : exp * exp -> exp (* TODO *)
  val fieldVar : exp * exp -> exp (* TODO *)

  val nilExp: unit -> exp
  val intExp: int -> exp
  val stringExp: string -> exp
  val callExp: Temp.label * exp list -> exp
  val opExp: Absyn.oper * exp * exp -> exp (* TODO: string comparisons *)
  val recordExp: exp list -> exp (* TODO: Matt *)
  val seqExp: exp list -> exp (* TODO: Matt *)
  val assignExp: exp * exp -> exp (* TODO: Matt *)
  val ifThenExp: exp * exp -> exp (* TODO: Matt *)
  val ifThenElse: exp * exp * exp -> exp (* TODO: Matt *)
  val whileExp: Temp.label * exp * exp -> exp (* TODO: Gabe *)
	val forExp: Temp.label * exp * exp * exp * exp -> exp (* TODO:Gabe *)
	val breakExp: Temp.label -> exp (* TODO:Gabe *)
  val arrayExp: exp * exp -> exp (* TODO: Gabe *)



end

structure Translate =
struct

  datatype level = TOP
             | LEVEL of {frame:Frame.frame, parent: level, unique unit ref}

  type access = level * Frame.access

  val outermost = TOP
  fun newLevel ({parent=p,name=n,formals=f}) = LEVEL({parent=p, frame=Frame.newFrame{name=n, formals=true::f}, unique=ref ()})
  fun formals (outermost) = []
    | formals (LEVEL({frame=f, parent=p, unique=u})) =
      let
        val a_list = (List.tl (Frame.formals f))
        fun make_access_list (element::list1, list2) = make_access_list (list1, (LEVEL({frame=f, parent=p, unique=u}),element)::list2)
          | make_access_list ([], list2) = list2
      in
        make_access_list (a_list, [])
      end

  fun allocLocal lvl bl = case lvl of
    LEVEL({frame=f, parent=p, unique=u}) => (lvl, Frame.allocLocal f bl)
    | outermost => (outermost, (Frame.allocLocal (Frame.newFrame {name=Temp.newlabel(), formals=[]}) bl)) (* TODO: can't do this *)

  val frags = ref []

  fun procEntryExit {level=l,body=b} =
    let
      val frame = case l of
        LEVEL({frame=f, parent=p, unique=u}) => f
        | outermost => Frame.newFrame ({name=Temp.newlabel(), formals=[]})
      val pbody = T.MOVE(T.TEMP(F.RV), unEx (my_body))
      val rbody = Frame.procEntryExit1(frame, pbody)
      val frag = Frame.PROC({body=rbody,frame=frame})
      val _ = (frags := frag::(!frags))
    in
      ()
    end

  fun getResult () = !frags

  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Temp.label * Temp.label -> T.stm

  fun unEx (Ex e) = e
    | unEx (Cx c) =
      let
        val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
      in
        T.ESEQ(T.SEQ[T.MOVE(T.TEMP r, T.CONST 1),c(t,f),T.LABEL f, T.MOVE(T.TEMP r, T.CONST 0), T.LABEL t], T.TEMP r)
      end
    | unEx (Nx n) = T.ESEQ(n, T.CONST 0)

  fun unCx(Cx c) = c
    | unCx(Ex(T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME f, [f]))
    | unCx(Ex(T.CONST _)) = (fn (t,f) => T.JUMP(T.NAME t, [t]))
    | unCx(Ex e) = (fn (t,f) => T.CJUMP(T.EQ, e, T.CONST 1, t, f))
    | unCx(Nx n) = (fn(t,f) => T.LABEL(Temp.newlabel())) (* TODO: Can't unCx an Nx *)

  fun unNx (Ex e) = T.EXP(e)
		| unNx (Cx c) = unNx (Ex (unEx (c)))
    | unNx (Nx n) = n

  fun nilExp () = Ex (T.CONST (0))
  fun intExp (num) = Ex (T.CONST (num))
  fun stringExp (str) =
    let
      (* TODO: Check if already exists in !frags *)
      val label = Temp.newlabel()
      val _ = (frags := Frame.STRING(label, str)::!frags)
    in
      Ex (T.NAME label)
    end

  fun callExp (label, exps) = Ex(T.CALL(T.NAME(label), map unEx exps))

  fun opExp (A.PlusOp, exp1, exp2) = Ex(T.BINOP(T.PLUS, unEx exp1, unEx exp2))
    | opExp (A.MinusOp, exp1, exp2) = Ex(T.BINOP(T.MINUS, unEx exp1, unEx exp2))
    | opExp (A.TimesOp, exp1, exp2) = Ex(T.BINOP(T.MUL, unEx exp1, unEx exp2))
    | opExp (A.DivideOp, exp1, exp2) = Ex(T.BINOP(T.DIV, unEx exp1, unEx exp2))
    | opExp (A.EqOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.EQ, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.NeqOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.NE, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.LtOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.LT, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.LeOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.LE, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.GtOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.GT, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.GeOp, exp1, exp2) = Cx(fn(t,f) => T.CJUMP(T.GE, unEx(exp1), unEx(exp2), t, f))

end
