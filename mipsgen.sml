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
      val calldefs = List.nth(F.specialregs,0)::List.nth(F.specialregs,3)::F.callersaves (* $ra, $rv, callersaves *)
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
        | munchStm (T.MOVE(T.TEMP i, e2)) = emit(A.OPER{assem="add $d0, $s0, $0\n", src=[munchExp e2], dst=[i], jump=NONE})
        | munchStm (T.MOVE(a,b)) = ()
        | munchStm (T.LABEL lab) = emit(A.LABEL{assem=(S.name lab) ^ ":\n", lab=lab})
        | munchStm (T.JUMP(T.NAME(lab),lablist)) = emit(A.OPER{assem="j " ^ (S.name lab) ^ "\n", src=[], dst=[], jump=SOME(lablist)})
        | munchStm (T.JUMP(e1,lablist)) = ()
        | munchStm (T.CJUMP(relop,e1,e2,l1,l2)) = emit(A.OPER{assem=T.relString(relop) ^ " $s0, $s1, " ^ (S.name l1) ^ "\n", src=[munchExp e1, munchExp e2], dst=[], jump=SOME[l1,l2]})
        | munchStm (T.EXP(T.CALL(T.NAME(lab), args))) = emit(A.OPER{assem="jal " ^ (S.name lab) ^"\n", src=munchArgs(0,args), dst=calldefs, jump=NONE})
        | munchStm (T.EXP(e)) = (munchExp e; ())

      and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = result(fn r => emit(A.OPER{assem="lw $d0, " ^ (Int.toString i) ^ "($s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = result(fn r => emit(A.OPER{assem="lw $d0, " ^ (Int.toString i) ^ "($s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.MEM(e1)) = result(fn r => emit(A.OPER{assem="lw $d0, 0($s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = result(fn r => emit(A.OPER{assem="addi $d0, $s0, " ^ (Int.toString i) ^ "\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = result(fn r => emit(A.OPER{assem="addi $d0, $s0, " ^ (Int.toString i) ^ "\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.CONST i) = result(fn r => emit(A.OPER{assem="addi $d0, $0, " ^ (Int.toString i) ^ "\n", src=[], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e1, e2)) = result(fn r => emit(A.OPER{assem="add $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS,e1,e2)) = result(fn r => emit(A.OPER{assem="sub $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MUL,e1,e2)) = result(fn r => emit(A.OPER{assem="mul $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.DIV,e1,e2)) = result(fn r => emit(A.OPER{assem="div $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND,e1,e2)) = result(fn r => emit(A.OPER{assem="and $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR,e1,e2)) = result(fn r => emit(A.OPER{assem="or $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.XOR,e1,e2)) = result(fn r => emit(A.OPER{assem="xor $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.LSHIFT,e1,e2)) = result(fn r => emit(A.OPER{assem="sllv $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.RSHIFT,e1,e2)) = result(fn r => emit(A.OPER{assem="srlv $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.ARSHIFT,e1,e2)) = result(fn r => emit(A.OPER{assem="srav $d0, $s0, $s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.ESEQ(s,e)) = (munchStm s; munchExp e)
        | munchExp (T.NAME(lab)) = result(fn r => emit(A.OPER{assem="la $d0, " ^ (S.name lab) ^ "\n", src=[], dst=[r], jump=NONE}))
        | munchExp (T.CALL(e,elist)) = result(fn r => emit(A.OPER{assem="", src=[], dst=[], jump=NONE}))
        | munchExp (T.TEMP t) = t

      and munchArgs (n, []) = []
        | munchArgs (n, (arg::args)) =
          let
            val argDst = if (n<4) then List.nth(F.argregs,n) else Temp.newtemp()
            val _ = if (n<4)
                    then emit(A.MOVE{assem="mv $a" ^ Int.toString n ^ ", 0($s0)\n", src=(munchExp arg), dst=argDst})
                    else emit(A.OPER{assem="sw $s0, " ^ Int.toString (4*(n-4)) ^ "($sp)\n", src=[munchExp (T.TEMP(List.nth(F.specialregs,2))), munchExp arg], dst=[], jump=NONE})
          in
            argDst::munchArgs(n+1,args)
          end

    in
      munchStm stm;
      rev(!ilist)
    end

end
