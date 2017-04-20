signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Map.map
  val alloc : Assem.instr list -> allocation
end

structure Reg_Alloc : REG_ALLOC =
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Map.map
  fun alloc instrs =
    let
      val fgraph = #1 (Makegraph.instrs2graph instrs)
      (*      val _ = (Flow.show fgraph) *)
      val igraph = #1 (Liveness.interferenceGraph fgraph)
      (* val _ = (Liveness.show igraph) *)
      val (allocation, spills) = Color.color {interference=igraph, initial=Frame.tempMap, registers=Frame.registers}
      (* return error if spill *)
      val _ = if List.length(spills)>0 then ErrorMsg.error 0 ("register allocation spill") else ()
    in
      allocation
    end
end
