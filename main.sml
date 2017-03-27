structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   (*structure R = RegAlloc*)

  fun emitproc out (F.PROC{body,frame}) =
    let
      val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
	    val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	    val instrs =   List.concat(map (Mips.codegen frame) stms')
      val format0 = Assem.format(Temp.makestring)
    in
      app (fn i => TextIO.output(out,format0 i)) instrs
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,s)

   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
	    handle e => (TextIO.closeOut out; raise e)
       end

       fun print_frags [] = ()
         | print_frags (frag::frags) = (case frag of
           F.PROC({body=body, frame=frame}) => (Printtree.printtree (TextIO.stdOut,body); print_frags frags)
           | F.STRING(label,string) => (print (string ^ "\n"); print_frags frags))

   fun compile filename =
       let val absyn = Parse.parse filename
           val frags = (Translate.resetFrags(); FindEscape.prog absyn; Semant.transProg absyn)
           (* val _ = print_frags frags *)
        in
            withOpenFile (filename ^ ".s")
	     (fn out => (app (emitproc out) frags))
       end

end
