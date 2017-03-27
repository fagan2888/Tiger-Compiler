signature CODEGEN =
sig
    structure F : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Mips : CODEGEN =
struct
  structure F = MipsFrame
  structure A = Assem
  structure T = Tree
  fun codegen frame stm = [] (* TODO *)

end
