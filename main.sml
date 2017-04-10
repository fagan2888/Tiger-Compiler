structure Main =
struct
  structure Tr = Translate
  structure F = MipsFrame
	structure S = Symbol

  fun emitproc out (F.PROC{body,frame}) =
    let
      val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
      val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      val instrs =   List.concat(map (MipsGen.codegen frame) stms') @ [Assem.OPER{assem="jr $ra\n", src=[],dst=[],jump=NONE}] (* TODO: fix ra after function *)
      val fgraph = #1 (Makegraph.instrs2graph instrs)
      (*val igraph = #1 (Liveness.interferenceGraph fgraph)
      val _ = (Liveness.show (TextIO.stdOut,igraph))*)
      val format0 = Assem.format(Temp.makestring)
    in
      app (fn i => TextIO.output(out,format0 i)) instrs
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,(S.name lab)^":\n" ^ s ^"\n")

  fun print_frags [] = ()
    | print_frags (frag::frags) = (case frag of
      MipsFrame.PROC({body=body, frame=frame}) => (Printtree.printtree (TextIO.stdOut,body); print_frags frags)
      | MipsFrame.STRING(label,string) => (print (string ^ "\n"); print_frags frags))

  fun withOpenFile fname f =
    let
      val out = TextIO.openOut fname
    in
      (f out before TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

  fun compile filename =
    let
      val _ = Translate.resetFrags()
      val ast = Parse.parse filename
			val _ = FindEscape.prog ast
      val frags = Semant.transProg(ast)
      (* val _ = if (!ErrorMsg.anyErrors) then () else (print_frags frags) *)
    in
      withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags))
    end

end
