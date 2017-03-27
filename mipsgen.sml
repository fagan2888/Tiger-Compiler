signature CODEGEN =
sig
    structure F : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure F = MipsFrame
  structure A = Assem
  structure S = Symbol
  structure T = Tree
  fun codegen frame stm =
    let
      val ilist = ref (nil: A.instr list)
      fun emit x = (ilist := x::(!ilist))
      fun result gen = let val t=Temp.newtemp() in gen t; t end

      fun munchStm (T.SEQ([])) = ()
        | munchStm (T.SEQ([a])) = munchStm a
        | munchStm (T.SEQ(a::l)) = (munchStm a; munchStm (T.SEQ(l)))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) = emit(A.OPER{assem="sw $s1, " ^ (Int.toString i) ^ "($s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) = emit(A.OPER{assem="sw $s1, " ^ (Int.toString i) ^ "($s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.CONST i),e2)) = emit(A.OPER{assem="sw s1, " ^ (Int.toString i) ^ "($s0)\n", src=[munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(e1), e2)) = emit(A.OPER{assem="sw s1, 0($s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.TEMP i, e2)) = emit(A.OPER{assem="add $d0, $s0, $0", src=[munchExp e2], dst=[i], jump=NONE})
        | munchStm (T.MOVE(a,b)) = ()
        | munchStm (T.LABEL lab) = emit(A.LABEL{assem=(S.name lab) ^ ":\n", lab=lab})
        | munchStm (T.JUMP(T.NAME(lab),lablist)) = emit(A.OPER{assem="j " ^ (S.name lab) ^ "\n", src=[], dst=[], jump=SOME(lablist)})
        | munchStm (T.JUMP(e1,lablist)) = ()
        | munchStm (T.CJUMP(relop,e1,e2,l1,l2)) = emit(A.OPER{assem=T.relString(relop) ^ " $s0, $s1, " ^ (S.name l1) ^ "\n", src=[munchExp e1, munchExp e2], dst=[], jump=SOME[l1,l2]})
        | munchStm (T.EXP(e)) = (munchExp e; ())

(* TODO *)
      and munchExp exp = result(fn r => emit(A.OPER{assem="", src=[], dst=[], jump=NONE}))
    in
      munchStm stm;
      rev(!ilist)
    end

end
