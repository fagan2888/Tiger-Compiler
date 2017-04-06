signature LIVENESS =
sig
  type igraph
  val interferenceGraph : Flow.flowgraph -> int (*igraph * (Temp.temp Flow.Graph.node -> Temp.temp list) *)
  val show : TextIO.outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct

  datatype igraph = IGRAPH of {graph: Temp.temp Flow.Graph.graph, tnode: Temp.temp -> Temp.temp Flow.Graph.node, gtemp: Temp.temp Flow.Graph.node -> Temp.temp, moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list}

  fun interferenceGraph (fg as Flow.FLOWGRAPH{control,def,use,ismove}) = 0

  fun show (outstream, IGRAPH{graph,tnode,gtemp,moves}) = Flow.Graph.printGraph (fn (nodeID,temp) => "Node " ^ (Int.toString nodeID) ^ ": " ^ (Temp.makestring temp)) graph
end
