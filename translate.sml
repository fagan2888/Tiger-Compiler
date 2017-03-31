structure Frame = MipsFrame

signature TRANSLATE =
sig
  type level
  type access
  type exp
  type frag
  val outermost: level
  val newLevel: {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val frags: frag list ref
  val resetFrags : unit -> unit
  val procEntryExit: {level: level, body: exp} -> unit
  val getResult: unit -> Frame.frag list

  val simpleVar: access * level -> exp
  val subscriptVar : exp * exp -> exp
	val fieldVar : exp * int -> exp

  val nilExp: unit -> exp
  val intExp: int -> exp
  val stringExp: string -> exp
  val callExp: Temp.label * exp list -> exp
  val opExp: Absyn.oper * exp * exp -> exp
  val recordExp: exp list -> exp
  val seqExp: exp list -> exp
  val assignExp: exp * exp -> exp
  val ifThenExp: exp * exp -> exp
  val ifThenElse: exp * exp * exp -> exp
  val whileExp: Temp.label * exp * exp -> exp
	val forExp: Temp.label * exp * exp * exp * exp -> exp
	val breakExp: Temp.label -> exp
  val arrayExp: exp * exp -> exp

end

structure Translate =
struct

  structure T = Tree
  structure A = Absyn

  datatype level = TOP
                 | LEVEL of {frame:Frame.frame, parent: level, unique: unit ref}

  type access = level * Frame.access

  type frag = Frame.frag

  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Temp.label * Temp.label -> T.stm

  val outermost = TOP

  fun newLevel ({parent=p,name=n,formals=f}) = LEVEL({parent=p, frame=Frame.newFrame{name=n, formals=true::f}, unique=ref ()})

  fun formals (TOP) = []
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
    | outermost => (ErrorMsg.error 0 ("allocLocal on outermost level"); (outermost, (Frame.allocLocal (Frame.newFrame {name=Temp.newlabel(), formals=[]}) bl)))

  val frags = ref [] : frag list ref

  fun resetFrags () = (frags := [])

  fun getResult () = !frags

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
    | unCx(Nx n) = (ErrorMsg.error 0 ("uncx an nx"); fn(t,f) => T.LABEL(Temp.newlabel()))

  fun unNx (Ex e) = T.EXP(e)
		| unNx (Cx c) = unNx (Ex (unEx (Cx c)))
    | unNx (Nx n) = n

  fun procEntryExit {level=l,body=b} =
    let
      val frame = case l of
        LEVEL({frame=f, parent=p, unique=u}) => f
        | outermost => Frame.newFrame ({name=Temp.newlabel(), formals=[]})
      val pbody = T.MOVE(T.TEMP(Frame.RV), unEx (b))
      val rbody = T.SEQ[T.LABEL(#name frame),Frame.procEntryExit1(frame, pbody)] (* TODO: function label fix *)
      val frag = Frame.PROC({body=rbody,frame=frame})
    in
      frags := frag::(!frags)
    end

  fun simpleVar ((lvl1, frm), lvl2) =
    let
      fun find_link (TOP,_) = (ErrorMsg.error 0 ("var on outermost level"); T.TEMP(Frame.FP))
        | find_link (_,TOP) = (ErrorMsg.error 0 ("var on outermost level"); T.TEMP(Frame.FP))
        | find_link (lvl1 as LEVEL({frame=f1, parent=p1, unique=u1}),LEVEL({frame=f2, parent=p2, unique=u2})) = if u1=u2 then T.TEMP(Frame.FP) else T.MEM(find_link(lvl1, p2))
    in
      Ex (Frame.exp frm (find_link(lvl1, lvl2)))
    end

	fun subscriptVar (v,exp) =
		let
				val var = unEx v;
				val ind = unEx exp;
        (* TODO: check bounds *)
		in
				Ex(T.MEM(T.BINOP(T.PLUS,T.MEM(var),T.BINOP(T.MUL,ind,T.CONST(Frame.wordsize)))))
		end

	fun fieldVar (v,num) = Ex(T.MEM(T.BINOP(T.PLUS,T.MEM(unEx v),T.CONST(num*Frame.wordsize))))

  fun nilExp () = Ex (T.CONST (0))
  fun intExp (num) = Ex (T.CONST (num))
  fun stringExp (str) =
    let
      val new_label = Temp.newlabel()
      fun search_frags [] = ((frags := Frame.STRING(new_label, str)::(!frags)); Ex (T.NAME new_label))
        | search_frags (frag::frags) = (case frag of
          Frame.STRING(label,string) => if str=string then Ex (T.NAME label) else (search_frags frags)
          | _ => search_frags frags)
    in
      search_frags (!frags)
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

  fun recordExp (exps) =
    let
      val r = Temp.newtemp()
      val init = [T.MOVE(T.TEMP(r),T.CALL(T.NAME(Temp.newlabel()),[T.CONST(List.length(exps)*Frame.wordsize)]))]
      fun create_seq (exp, list) = T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP(r),T.CONST((List.length(list)-1)*Frame.wordsize))),unEx(exp))::list
      val rec_seq = foldl create_seq init exps
    in
      Ex(T.ESEQ(T.SEQ(rec_seq),T.TEMP(r)))
    end

  fun seqExp [] = Ex (T.CONST 0)
    | seqExp [exp] = exp
    | seqExp (exps) = Ex(T.ESEQ(T.SEQ(map unNx (List.take(exps,List.length(exps)-1))), unEx (List.last(exps))))

  fun assignExp (exp1,exp2) = Nx (T.MOVE (unEx exp1, unEx exp2))

  fun ifThenExp (exp1,exp2) =
    let
      val r = Temp.newtemp()
      val t = Temp.newlabel() and f = Temp.newlabel()
    in
      case exp2 of
        Cx c => Cx(fn(tt,ff) => (T.SEQ[(unCx(exp1)(t,f)),T.LABEL(t),c(tt,ff),T.LABEL(f)]))
        | Nx n => Nx(T.SEQ[(unCx(exp1)(t,f)),T.LABEL(t),n,T.LABEL(f)])
        | Ex e => Ex(T.ESEQ(T.SEQ[(unCx(exp1)(t,f)),T.LABEL(t),T.MOVE(T.TEMP(r),e),T.LABEL(f)], T.TEMP(r)))
    end

  fun ifThenElseExp (exp1,exp2,exp3) =
    let
      val r = Temp.newtemp()
      val t = Temp.newlabel() and f = Temp.newlabel() and j = Temp.newlabel()
    in
      case (exp2,exp3) of
        (Cx c2, Cx c3) => Cx(fn(tt,ff) => (T.SEQ[(unCx exp1)(t, f),T.LABEL t,c2 (tt, ff),T.LABEL f,c3 (tt, ff)]))
        | (Nx n2, Nx n3) => Nx(T.SEQ[(unCx exp1)(t,f),T.LABEL t,n2,T.JUMP(T.NAME j, [j]),T.LABEL f,n3,T.LABEL j])
        | (Ex e2, Ex e3) => Ex(T.ESEQ(T.SEQ[(unCx exp1)(t,f),T.LABEL t,T.MOVE(T.TEMP r,e2),T.JUMP(T.NAME j,[j]),T.LABEL f,T.MOVE(T.TEMP r,e3),T.LABEL j], T.TEMP(r)))
        | (_,_) => Ex (T.CONST (0)) (* then and else differ *)
    end

  fun whileExp (break,cond,body) =
    let
      val l1 = Temp.newlabel() and l2 = Temp.newlabel()
    in
      Nx (T.SEQ[T.JUMP(T.NAME l1,[l1]),T.LABEL l2,unNx body,T.LABEL l1, unCx cond (l2,break),T.LABEL break])
    end

  fun forExp (break, var, lo, hi, exp) =
    let
      val l1 = Temp.newlabel() and l2 = Temp.newlabel() and l3 = Temp.newlabel()
    in
      Nx (T.SEQ[T.MOVE(unEx var, unEx lo),T.CJUMP(T.LE, unEx var, unEx hi, l2, l3),T.LABEL l1,T.MOVE(unEx var, T.BINOP(T.PLUS, unEx var, T.CONST(1))), T.LABEL l2, unNx exp, T.CJUMP(T.LT, unEx var, unEx hi, l1, l3), T.LABEL l3])
    end

  fun breakExp (break) = Nx (T.JUMP(T.NAME break,[break]))

	fun recordExp (exps) =
    let
      val r = Temp.newtemp()
      val init = [T.MOVE(T.TEMP(r),T.CALL(T.NAME(Temp.newlabel()),[T.CONST(List.length(exps)*Frame.wordsize)]))]
      fun create_seq (exp, list) = T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP(r),T.CONST((List.length(list)-1)*Frame.wordsize))),unEx(exp))::list
      val rec_seq = foldl create_seq init exps
    in
      Ex(T.ESEQ(T.SEQ(rec_seq),T.TEMP(r)))
    end


	fun arrayExp (exp1, exp2) =
		let
				val r = Temp.newtemp();
				val size = unEx(exp1);
				val init = unEx(exp2);
			in
				Ex(T.ESEQ(T.SEQ([T.MOVE(T.TEMP(r),Frame.externalCall("tiger_init_array",[size,init]))]),T.TEMP(r)))
		end
end
