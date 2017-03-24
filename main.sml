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
      val frags = Semant.transProg(ast) (* TODO: call exp with more than 4*)
      val _ = print_frags frags
    in
      ()
    end

end
