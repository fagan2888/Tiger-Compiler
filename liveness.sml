signature LIVENESS =
sig
  type igraph
  val interferenceGraph : Flow.flowgraph -> int (*igraph * (Temp.temp G.node -> Temp.temp list) *)
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct

  datatype igraph = IGRAPH of {graph: Temp.temp G.graph, tnode: Temp.temp -> Temp.temp G.node, gtemp: Temp.temp G.node -> Temp.temp, moves: (Temp.temp G.node * Temp.temp G.node) list}

  fun interferenceGraph (fg as Flow.FLOWGRAPH{control,def,use,ismove}) = 0

  fun show (outstream, IGRAPH{graph,tnode,gtemp,moves}) = G.printGraph (fn (nodeID,temp) => "Node " ^ (Int.toString nodeID) ^ ": " ^ (Temp.makestring temp)) graph
end
