signature FRAME =
sig
  type frame
  type access
  val newFrame : {name : Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val RV : Temp.temp
  val wordsize : int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp

  type register
  val tempMap: register Temp.Map.map
  val tempString: Temp.temp -> string
  val specialregs : Temp.temp list
  val argregs : Temp.temp list
  val calleesaves : Temp.temp list
  val callersaves : Temp.temp list

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end
