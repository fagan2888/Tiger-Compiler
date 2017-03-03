structure Main =
struct

  fun compile filename =
    let
      val ast = Parse.parse filename
      val tych = Semant.transProg(ast)
    in
      ast
    end

end
