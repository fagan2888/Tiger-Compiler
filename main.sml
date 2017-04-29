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
      val instrs = List.concat(map (MipsGen.codegen frame) stms')
      val instrs' = F.procEntryExit2 (frame, instrs)
      val allocation = Reg_Alloc.alloc instrs'
      val instrs'' = F.procEntryExit3 (frame, instrs')
      val format0 = Assem.format(fn (temp) => case Temp.Map.find(allocation,temp) of SOME str => ("$" ^ str) | NONE => ("$ERROR"))
    in
				TextIO.output(out,".text\n");
				app (fn i => TextIO.output(out,format0 i)) (#body instrs'');
				TextIO.output(out,".data\n")
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,(S.name lab)^":\n" ^ ".asciiz \"" ^ s ^"\"\n")

  fun print_frags [] = ()
    | print_frags (frag::frags) = (case frag of
      MipsFrame.PROC({body=body, frame=frame}) => (Printtree.printtree (TextIO.stdOut,body); print_frags frags)
      | MipsFrame.STRING(label,string) => (print (string ^ "\n"); print_frags frags))

  fun withOpenFile fname f =
    let
      val out = TextIO.openAppend fname
    in
      (f out before TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

	fun copyFile (infile,outfile,append) =
		let
				val ins = TextIO.openIn infile
				val outs = if append then TextIO.openAppend outfile else TextIO.openOut outfile

				fun helper(copt:char option) =
					case copt of
							NONE => (TextIO.closeIn ins; TextIO.closeOut outs)
					 |  SOME(c) => (TextIO.output1(outs,c); helper(TextIO.input1 ins))
		in
				helper(TextIO.input1 ins)
		end


  fun compile filename =
    let
      val _ = Translate.resetFrags()
      val ast = Parse.parse filename
			val _ = FindEscape.prog ast
      val frags = Semant.transProg(ast)
      (* val _ = if (!ErrorMsg.anyErrors) then () else (print_frags frags) *)
    in
				copyFile ("runtimele.s",filename^".s",false);
				copyFile ("sysspim.s",filename^".s",true);
				withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags))
    end

end
