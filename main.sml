structure Main =
struct

  fun compile filename =
    let
      fun print_frags [] = ()
        | print_frags (frag::frags) = (case frag of
          MipsFrame.PROC({body=body, frame=frame}) => (Printtree.printtree (TextIO.stdOut,body); print_frags frags)
          | MipsFrame.STRING(label,string) => (print (string ^ "\n"); print_frags frags))
      val _ = Translate.resetFrags()
      val ast = Parse.parse filename
			val _ = FindEscape.findEscape ast
      val frags = Semant.transProg(ast)
      val _ = print_frags frags
    in
      ()
    end

end
